In this folder there are some example certificates and keys for testing
purposes. Don't use these in real life.

## Encode Certificate Authority

    cfssl gencert -initca ca.json | cfssljson -bare ca

## decode public key

    openssl x509 -in ca.pem -text -noout

## Create host self sertificate

```bash
cfssl gencert \
  -ca=ca.pem \
  -ca-key=ca-key.pem \
  -config=ca-config.json \
  -profile=devops \
  localhost.json | cfssljson -bare localhost
```

## decode host public key

    openssl x509 -in localhost.pem -text -noout

## What pass to server on launch?

```bash
stack run titan -- \
  --cert=certs/localhost.pem \
  --key=certs/localhost-key.pem \
  --cacert=certs/ca.pem \
  localhost
```
