module Lib.Commands
    (
        genPrivateKey
        , genRootPrivateKey
        , genCsr
        , genRootCsr
        , genCert
        , genSignedCert
        , genRootCert
        , genServerCert
        , genClientCert
    )

where

import           Control.Monad.Trans.Either (EitherT, left, right)
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as Text
import           Lib.Env (withEnv)
import           Lib.Proc (sysOut, sysEitherT)

type PrivateKeyBits = Int

genPrivateKey :: FilePath -> PrivateKeyBits -> [Text] -> EitherT String IO ()
genPrivateKey path bits args =
    sysEitherT
        (sysOut
            "openssl"
            (["genrsa", "-out", Text.pack path] <> args <> [Text.pack $ show bits]))
        (\ out ->
            let err = "Can't generate key at " <> path <> Text.unpack out
            in left err)
        (const $ right ())

genRootPrivateKey :: EitherT String IO ()
genRootPrivateKey = genPrivateKey "private/ca.key" 4096 mempty

genCsr :: FilePath -> FilePath -> Text -> [Text] -> EitherT String IO ()
genCsr key path subject args =
    sysEitherT
        (sysOut
            "openssl"
            ([
                "req"
                , "-key", Text.pack key
                , "-new"
                , "-out", Text.pack path
                , "-subj", subject
             ] <> args))
        (\ out ->
            let err = "Failed to generate CSR at " <> path <> Text.unpack out
            in left err)
        (const $ right ())

genRootCsr :: EitherT String IO ()
genRootCsr =
    genCsr
        "private/ca.key"
        "ca.csr"
        "/C=GB/ST=England/O=Kubernetes Cluster/CN=Kubernetes Cluster Root CA"
        ["-config", "openssl.cnf"]

genCert :: FilePath -> FilePath -> FilePath -> [Text] -> EitherT String IO ()
genCert key csr path args =
    sysEitherT
        (sysOut
            "openssl"
            ([
                "req"
                , "-key", Text.pack key
                , "-in", Text.pack csr
                , "-out", Text.pack path
                , "-x509"
                , "-sha256"
            ] <> args))
        (\ out ->
            let err = "Unable to generate cert at " <> path <> Text.unpack out
            in left err)
        (const $ right ())

genSignedCert :: FilePath -> FilePath -> FilePath -> [Text] -> EitherT String IO ()
genSignedCert key csr path args =
    sysEitherT
        (sysOut
            "openssl"
            ([
                "ca"
                , "-key", Text.pack key
                , "-in", Text.pack csr
                , "-out", Text.pack path
                , "-notext"
                , "-batch"
                , "-md", "sha256"
            ] <> args))
        (\ out ->
            let err = "Unable to generate cert at " <> path <> Text.unpack out
            in left err)
        (const $ right ())

genRootCert :: EitherT String IO ()
genRootCert =
    genCert
        "private/ca.key"
        "ca.csr"
        "certs/ca.crt"
        ["-days", "7300", "-config", "openssl.cnf", "-extensions", "v3_ca"]

genServerCert :: FilePath
              -> FilePath
              -> FilePath
              -> [Text]
              -> EitherT String IO ()
genServerCert key csr path sans =
    withEnv [("subjectAltName", Text.unpack $ Text.intercalate "," sans)] $
        genSignedCert
           key
           csr
           path
           [ "-days" , "375"
           , "-config" , "openssl-san.cnf"
           , "-extensions" , "server_cert"
           ]

genClientCert :: FilePath
              -> FilePath
              -> FilePath
              -> EitherT String IO ()
genClientCert key csr path =
    withEnv [("subjectAltName", " ")] $
        genSignedCert
           key
           csr
           path
           [ "-days" , "375"
           , "-config" , "openssl-san.cnf"
           , "-extensions" , "usr_cert"
           ]
