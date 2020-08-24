# Titan

Titan is a [gemini](https://gemini.circumlunar.space/) server whose design is based on [Servant](https://www.servant.dev/).

# How to use

Look at `app/` for an example project.

Titan uses a reduced set of route combinators from Servant:
```
Get a
Capture a
a :> b
:<|>
QueryParam sym a
QueryFlag sym
```

## Example routes
```
type MyAPI =
    -- GET /date returns a value of type `Day`
          "date" :> Get Day
    -- GET /time/:timezone returns a value of type `ZonedTime`
    :<|> "time" :> Capture Timezone :> Get ZonedTime
    -- GET /add?x={0-9*}&y={0-9*} Returns a value of type `Int`
    :<|> "add" :> QueryParam "x" Int :> QueryParam "y" Int :> Get Int
    -- GET /books[?published] Returns a value of type `[Text]`
    :<|> "books" :> QueryFlag "published" :> Get [Text]

-- Newtype wrapper to prevent an orphan instance
newtype Timezone = Timezone TimeZone
  deriving Read

instance FromHttpApiData Timezone where
  parseUrlPiece t = Timezone <$> (readTextData t :: Either Text TimeZone)

-- There is one handler per endpoint. 
handleDate :: Server (Get Day)
handleDate = do
  date <- liftIO $ utctDay <$> getCurrentTime
  pure $ Response (Header Two "text/gemini") (Just date)

handleTime :: Server (Capture Timezone :> Get ZonedTime)
handleTime (Timezone tz) = do
  time <- liftIO $ utcToZonedTime tz <$> getCurrentTime
  pure $ Response (Header Two "text/gemini") (Just time)

handleAdd :: Server (QueryParam "x" Int :> QueryParam "y" Int :> Get Int)
handleAdd (Just x) (Just y) =
  pure $ Response (Header Two "text/gemini") (Just (x + y))
handleAdd Nothing Nothing = throwError $ (Five, "Bad values for 'x' and 'y'")
handleAdd Nothing _ = throwError $ (Five, "Bad values for 'x'")
handleAdd _ Nothing = throwError $ (Five, "Bad values for 'y'")

handleBook :: Server (QueryFlag "published" :> Get [Text])
handleBook published =
  let books = [ ("One Flew Over the Cuckcoo's Next", True)
              , ("The Autobiography of Alice B. Toklas", True)
              , ("The Dead Sea Scrolls", False)
              , ("The Light House", False)]
  in if published
     then pure $ Response (Header Two "text/gemini") $
          Just $ fmap fst $ filter (\p -> not $ snd p) books
     else pure $ Response (Header Two "text/gemini") $
          Just $ fmap fst books

-- Handlers run in the `ExceptT (ResponseCode, Text) IO` Monad
handleMyAPI :: Server MyAPI
handleMyAPI = handleDate :<|> handleTime :<|> handleAdd :<|> handleBook

-- Run the server with `runServer`
main :: IO ()
main = Z.withSocketsDo $ do
  args <- getArgs >>= parseArgs
  runReaderT (runServer (Proxy :: Proxy MyAPI) handleMyAPI) (makeContext args)
```

## Run the server
```
cabal exec titan -- --cert=certs/server.example.com.crt --key=certs/server.example.com.key --cacert=certs/ca.example.com.crt localhost
```
