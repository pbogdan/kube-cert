{-# LANGUAGE FlexibleContexts #-}

module Main where

import           Control.Lens hiding (Context)
import           Control.Monad (forM)
import           Control.Monad (forM_)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Either
import qualified Data.ByteString.Lazy.Char8 as ByteChar
import           Data.IP
import           Data.Monoid (mempty, (<>))
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as Text
import qualified Data.Text.Lazy as LazyText
import           Data.Text.Template
import           Lib
import           Lib.Env (withEnv)
import           Lib.Opts (Opts(..), optsParser, defaultOpts)
import           Network.Wreq
import           Options.Applicative (execParser)
import           Options.Applicative (fullDesc)
import           Options.Applicative (helper)
import           Options.Applicative (info)
import           System.Directory.Extra
import           System.Environment
import           System.FilePath.Posix
import           System.IO.Temp
import           System.Posix.Files
import           System.Posix.User
import           System.Process.Extra
import qualified Turtle

data MasterIp
    = IpAddress !String
    | GceExternalIp
    | AwsExternalIp
    | AzureDnsName
    deriving (Show)

parseMasterIP :: String -> MasterIp
parseMasterIP ip = case ip of
    "_use_gce_external_ip_" -> GceExternalIp
    "_use_aws_external_ip_" -> AwsExternalIp
    "_use_azure_dns_name_"  -> AzureDnsName
    _                       -> IpAddress ip

retrieveIp :: (MonadIO m) => MasterIp -> m IPv4
retrieveIp ip = case ip of
    IpAddress address -> parseIp address
    GceExternalIp     -> fetchGceIp
    AwsExternalIp     -> fetchAwsIp
    AzureDnsName      -> fetchAzureIp

parseIp :: (MonadIO m) => String -> m IPv4
parseIp s = return (read s :: IPv4)

-- @TODO: implement connection timeout
fetchGceIp :: (MonadIO m) => m IPv4
fetchGceIp = do
    let opts = defaults & header "Metadata-Flavor" .~ ["Google"]
    r <- liftIO $ getWith opts "http://metadata.google.internal./computeMetadata/v1/instance/network-interfaces/0/access-configs/0/external-ip"
    return $! (read (ByteChar.unpack (r ^. responseBody)) :: IPv4)

fetchAwsIp :: (MonadIO m) => m IPv4
fetchAwsIp = do
    r <- liftIO $ get "http://169.254.169.254/latest/meta-data/public-ipv4"
    return $! (read (ByteChar.unpack (r ^. responseBody)) :: IPv4)

fetchAzureIp :: (MonadIO m) => m IPv4
fetchAzureIp = error "not implemented"

-- extract first IP address of an IP range, with IP range being specified as
-- address + mask. We extract the IP range address and request first element via
-- its Enum instance
firstIpInRange :: AddrRange IPv4 -> IPv4
firstIpInRange = succ . addr

defaultSans :: Opts -> IO [Text]
defaultSans opts = do
    certIp <- retrieveIp (parseMasterIP (optsMasterIp opts))
    return $ 
        [ "IP:" <> (T.pack $ show certIp)
        , "IP:" <> (T.pack $ show (firstIpInRange (read (optsServiceClusterIpRange opts) :: AddrRange IPv4)))
        , "DNS:kubernetes"
        , "DNS:kubernetes.default"
        , "DNS:kubernetes.default.svc"
        , "DNS:kubernetes.default.svc." <> (T.pack (optsDnsDomain opts))
        , "DNS:" <> (T.pack $ (optsMasterName opts))]

context :: [(T.Text, T.Text)] -> Context
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
            LazyText.toStrict $ substitute tpl $ context [("dir", T.pack path)]
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
            (["genrsa", "-out", T.pack path] <> args <> [T.pack $ show bits])
            Turtle.empty
    case ret of
        Turtle.ExitSuccess -> right ()
        Turtle.ExitFailure _ ->
            let err = "Can't generate key at " <> path <> T.unpack out
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
                , "-key", T.pack key
                , "-new"
                , "-out", T.pack path
                , "-subj", subject
             ] <> args)
            Turtle.empty

    case ret of
        Turtle.ExitSuccess   -> right ()
        Turtle.ExitFailure _ ->
            let err = "Failed to generate CSR at " <> path <> T.unpack out
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
                , "-key", T.pack key
                , "-in", T.pack csr
                , "-out", T.pack path
                , "-x509"
                , "-sha256"
            ] <> args)
            Turtle.empty
    case ret of
        Turtle.ExitSuccess   -> right ()
        Turtle.ExitFailure _ -> 
            let err = "Unable to generate cert at " <> path <> T.unpack out
            in left err

genSignedCert :: FilePath -> FilePath -> FilePath -> [Text] -> EitherT String IO ()
genSignedCert key csr path args = do
    (ret, out) <-
        Turtle.procStrict
            "openssl"
            ([
                "ca"
                , "-key", T.pack key
                , "-in", T.pack csr
                , "-out", T.pack path
                , "-notext"
                , "-batch"
                , "-md", "sha256"
            ] <> args)
            Turtle.empty
    case ret of
        Turtle.ExitSuccess   -> right ()
        Turtle.ExitFailure _ -> 
            let err = "Unable to generate cert at " <> path <> T.unpack out
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
    withEnv [("subjectAltName", T.unpack $ T.intercalate "," sans)] $ do
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
