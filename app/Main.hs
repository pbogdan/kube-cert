module Main where

import           Control.Monad (forM, forM_)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Either
import           Data.IP
import           Data.Monoid (mempty, (<>))
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Text.Lazy as LazyText
import           Data.Text.Template
import           Lib
import           Lib.Env (withEnv)
import           Lib.IP (retrieveIp, parseMasterIP, firstIpInRange)
import           Lib.Opts (Opts(..), optsParser, defaultOpts)
import           Options.Applicative (execParser, fullDesc, helper, info)
import           System.Directory.Extra
import           System.Environment
import           System.FilePath.Posix
import           System.IO.Temp
import           System.Posix.Files
import           System.Posix.User
import           System.Process.Extra
import qualified Turtle

defaultSans :: Opts -> IO [Text]
defaultSans opts = do
    certIp <- retrieveIp (parseMasterIP (optsMasterIp opts))
    return $ 
        [ "IP:" <> (Text.pack $ show certIp)
        , "IP:" <> (Text.pack $ show (firstIpInRange (read (optsServiceClusterIpRange opts) :: AddrRange IPv4)))
        , "DNS:kubernetes"
        , "DNS:kubernetes.default"
        , "DNS:kubernetes.default.svc"
        , "DNS:kubernetes.default.svc." <> (Text.pack (optsDnsDomain opts))
        , "DNS:" <> (Text.pack $ (optsMasterName opts))]

context :: [(Text, Text)] -> Context
context assocs x = maybe err id . lookup x $ assocs
  where err = error $ "Could not find key: " ++ show x

initCA :: Opts -> IO ()
initCA opts =
    withSystemTempDirectory "kube-cert" $ \tmpdir -> do
        genOpenSslConfig openSslTemplate (tmpdir </> "openssl.cnf")
        genOpenSslConfig openSslSanTemplate (tmpdir </> "openssl-san.cnf")

        withCurrentDirectory tmpdir $ do
            forM_ ["certs", "crl", "newcerts", "private"] $ \subdir ->
                 createDirectory subdir
            Text.writeFile "index.txt" ""
            Text.writeFile "serial" "1000"

            sans <- defaultSans opts
        
            r <- runEitherT $ do
                -- root stuff
                genRootPrivateKey
                genRootCsr
                genRootCert

                -- server cert
                genPrivateKey "private/server.key" 2048 mempty
                genCsr
                    "private/server.key"
                    "server.csr"
                    "/C=GB/ST=England/O=Kubernetes Cluster/CN=lol.com"
                    ["-config", "openssl.cnf"]
                genServerCert 
                    "private/server.key"
                    "server.csr"
                    "certs/server.crt"
                    sans

                -- kubelet cert
                genPrivateKey "private/kubelet.key" 2048 mempty
                genCsr
                    "private/kubelet.key"
                    "kubelet.csr"
                    "/C=GB/ST=England/O=Kubernetes Cluster/CN=kubelet"
                    ["-config", "openssl.cnf"]
                genClientCert
                    "private/kubelet.key"
                    "kubelet.csr"
                    "certs/kubelet.crt"

                -- kubecfg cert
                genPrivateKey "private/kubecfg.key" 2048 mempty
                genCsr
                    "private/kubecfg.key"
                    "kubecfg.csr"
                    "/C=GB/ST=England/O=Kubernetes Cluster/CN=kubecfg"
                    ["-config", "openssl.cnf"]
                genClientCert
                    "private/kubecfg.key"
                    "kubecfg.csr"
                    "certs/kubecfg.crt"

            case r of
                Right () -> putStrLn "Generated some crap"
                Left err -> putStrLn $ "Generating crap failed: " <> err
            -- Turtle.proc "openssl" ["x509", "-noout", "-text", "-in", "certs/ca.cert.pem"] Turtle.empty
            -- Turtle.proc "openssl" ["req", "-noout", "-text", "-in", "ca.cert.csr"] Turtle.empty
            -- Turtle.proc "openssl" ["x509", "-noout", "-text", "-in", "certs/server.crt"] Turtle.empty

            print =<< Text.readFile "index.txt"

            copyFile "certs/ca.cert.pem" ((optsCertDir opts) </> "ca.crt")
            copyFile "private/ca.key.pem" ((optsCertDir opts) </> "ca.key")

            copyFile "certs/server.crt" ((optsCertDir opts) </> "server.crt")
            copyFile "certs/kubelet.crt" ((optsCertDir opts) </> "kubelet.crt")
            copyFile "certs/kubecfg.crt" ((optsCertDir opts) </> "kubecfg.crt")

            copyFile "private/server.key" ((optsCertDir opts) </> "server.key")
            copyFile "private/kubelet.key" ((optsCertDir opts) </> "kubelet.key")
            copyFile "private/kubecfg.key" ((optsCertDir opts) </> "kubecfg.key")

            gid <- groupID <$> getGroupEntryForName (optsCertGroup opts)
            setOwnerAndGroup ((optsCertDir opts) </> "ca.crt")      (-1) gid
            setOwnerAndGroup ((optsCertDir opts) </> "ca.key")      (-1) gid
            setOwnerAndGroup ((optsCertDir opts) </> "server.crt")  (-1) gid
            setOwnerAndGroup ((optsCertDir opts) </> "kubelet.crt") (-1) gid
            setOwnerAndGroup ((optsCertDir opts) </> "kubecfg.crt") (-1) gid
            setOwnerAndGroup ((optsCertDir opts) </> "server.key")  (-1) gid
            setOwnerAndGroup ((optsCertDir opts) </> "kubelet.key") (-1) gid
            setOwnerAndGroup ((optsCertDir opts) </> "kubecfg.key") (-1) gid

            let perms = foldr unionFileModes nullFileMode [ownerReadMode, ownerWriteMode, groupReadMode, groupWriteMode]

            setFileMode ((optsCertDir opts) </> "ca.crt")      perms
            setFileMode ((optsCertDir opts) </> "ca.key")      perms
            setFileMode ((optsCertDir opts) </> "server.crt")  perms
            setFileMode ((optsCertDir opts) </> "kubelet.crt") perms
            setFileMode ((optsCertDir opts) </> "kubecfg.crt") perms
            setFileMode ((optsCertDir opts) </> "server.key")  perms
            setFileMode ((optsCertDir opts) </> "kubelet.key") perms
            setFileMode ((optsCertDir opts) </> "kubecfg.key") perms

            Turtle.proc "openssl" ["x509", "-noout", "-text", "-in", "certs/kubelet.crt"] Turtle.empty
            Turtle.proc "openssl" ["x509", "-noout", "-text", "-in", "certs/kubecfg.crt"] Turtle.empty

            return ()

getOpenSslConfig :: Text -> FilePath -> Text
getOpenSslConfig tpl path =
    let tt =
            LazyText.toStrict $ substitute tpl $ context [("dir", Text.pack path)]
    in tt

genOpenSslConfig :: Text -> FilePath -> IO ()
genOpenSslConfig tpl path = do
    _ <- Text.writeFile path (getOpenSslConfig tpl (takeDirectory path))
    return ()

type PrivateKeyBits = Int

genPrivateKey :: FilePath -> PrivateKeyBits -> [Text] -> EitherT String IO ()
genPrivateKey path bits args = do
    (ret,out) <-
        Turtle.procStrict
            "openssl"
            (["genrsa", "-out", Text.pack path] <> args <> [Text.pack $ show bits])
            Turtle.empty
    case ret of
        Turtle.ExitSuccess -> right ()
        Turtle.ExitFailure _ ->
            let err = "Can't generate key at " <> path <> Text.unpack out
            in left err

genRootPrivateKey :: EitherT String IO ()
genRootPrivateKey = genPrivateKey "private/ca.key.pem" 4096 mempty

genCsr :: FilePath -> FilePath -> Text -> [Text] -> EitherT String IO ()
genCsr key path subject args = do
    (ret,out) <-
        Turtle.procStrict
            "openssl"
            ([
                "req"
                , "-key", Text.pack key
                , "-new"
                , "-out", Text.pack path
                , "-subj", subject
             ] <> args)
            Turtle.empty

    case ret of
        Turtle.ExitSuccess   -> right ()
        Turtle.ExitFailure _ ->
            let err = "Failed to generate CSR at " <> path <> Text.unpack out
            in left err

genRootCsr :: EitherT String IO ()
genRootCsr =
    genCsr
        "private/ca.key.pem"
        "ca.cert.csr"
        "/C=GB/ST=England/O=Kubernetes Cluster/CN=Kubernetes Cluster Root CA"
        ["-config", "openssl.cnf"]

genCert :: FilePath -> FilePath -> FilePath -> [Text] -> EitherT String IO ()
genCert key csr path args = do
    (ret, out) <-
        Turtle.procStrict
            "openssl"
            ([
                "req"
                , "-key", Text.pack key
                , "-in", Text.pack csr
                , "-out", Text.pack path
                , "-x509"
                , "-sha256"
            ] <> args)
            Turtle.empty
    case ret of
        Turtle.ExitSuccess   -> right ()
        Turtle.ExitFailure _ -> 
            let err = "Unable to generate cert at " <> path <> Text.unpack out
            in left err

genSignedCert :: FilePath -> FilePath -> FilePath -> [Text] -> EitherT String IO ()
genSignedCert key csr path args = do
    (ret, out) <-
        Turtle.procStrict
            "openssl"
            ([
                "ca"
                , "-key", Text.pack key
                , "-in", Text.pack csr
                , "-out", Text.pack path
                , "-notext"
                , "-batch"
                , "-md", "sha256"
            ] <> args)
            Turtle.empty
    case ret of
        Turtle.ExitSuccess   -> right ()
        Turtle.ExitFailure _ -> 
            let err = "Unable to generate cert at " <> path <> Text.unpack out
            in left err


genRootCert :: EitherT String IO ()
genRootCert =
    genCert
        "private/ca.key.pem"
        "ca.cert.csr"
        "certs/ca.cert.pem"
        ["-days", "7300", "-config", "openssl.cnf", "-extensions", "v3_ca"]

genServerCert :: FilePath
              -> FilePath
              -> FilePath
              -> [Text]
              -> EitherT String IO ()
genServerCert key csr path sans =
    withEnv [("subjectAltName", Text.unpack $ Text.intercalate "," sans)] $ do
        liftIO $ putStrLn =<< getEnv "subjectAltName"
        genSignedCert
           key
           csr
           path
           [ "-days" , "375"
           , "-config" , "openssl-san.cnf"
           , "-extensions" , "server_cert"
           ]

genClientCert  :: FilePath
              -> FilePath
              -> FilePath
              -> EitherT String IO ()
genClientCert key csr path =
    withEnv [("subjectAltName", " ")] $ do
        genSignedCert
           key
           csr
           path
           [ "-days" , "375"
           , "-config" , "openssl-san.cnf"
           , "-extensions" , "usr_cert"
           ]

main :: IO ()
main = do
    opts <- execParser (info (helper <*> optsParser) (fullDesc <> mempty))
    initCA opts
    return ()

{- 

list of supported command line arguments:

--master-ip
--master-name
--dns-domain
--service-cluster-ip-range
--cert-dir
--cert-group

master ip can be just an IP address or one of the special values:

- _use_gce_external_ip_
- _use_aws_external_ip_
- _use_azure_dns_name_

-}
