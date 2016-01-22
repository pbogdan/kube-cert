module Main where

import           Control.Monad (forM_)
import           Control.Monad.Trans.Either
import           Data.IP
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import           Lib.Commands
import           Lib.IP (retrieveIp, parseMasterIP, firstIpInRange)
import           Lib.Opts (Opts(..), optsParser, defaultOpts)
import           Lib.Template (openSslTemplate, openSslSanTemplate, genOpenSslConfig)
import           Options.Applicative (execParser, fullDesc, helper, info)
import           System.Directory.Extra
import           System.FilePath.Posix
import           System.IO.Temp
import           System.Posix.Files
import           System.Posix.User

defaultSans :: Opts -> IO [Text]
defaultSans opts = do
    certIp <- retrieveIp (parseMasterIP (optsMasterIp opts))
    return 
        [ "IP:" <> Text.pack (show certIp)
        , "IP:" <> Text.pack (show (firstIpInRange (read (optsServiceClusterIpRange opts) :: AddrRange IPv4)))
        , "DNS:kubernetes"
        , "DNS:kubernetes.default"
        , "DNS:kubernetes.default.svc"
        , "DNS:kubernetes.default.svc." <> Text.pack (optsDnsDomain opts)
        , "DNS:" <> Text.pack (optsMasterName opts)]

initCA :: FilePath -> IO ()
initCA root =
    withCurrentDirectory root $ do
        let templates =
                [ (openSslTemplate,    "openssl.cnf")
                , (openSslSanTemplate, "openssl-san.cnf")]
         
        forM_ templates $ \ (action, tplpath) ->
            genOpenSslConfig action tplpath

        forM_ ["certs", "crl", "newcerts", "private"] $ \ subdir ->
            createDirectory subdir
        
        Text.writeFile "index.txt" ""
        Text.writeFile "serial" "1000"

setupCA :: EitherT String IO ()
setupCA = do
    genRootPrivateKey
    genRootCsr
    genRootCert

setupServerCert :: [Text] -> EitherT String IO ()
setupServerCert sans = do
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

setupKubeletCert :: EitherT String IO ()
setupKubeletCert = do
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

setupKubecfgCert  :: EitherT String IO ()
setupKubecfgCert = do
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

generateCerts :: Opts -> IO ()
generateCerts opts =
    withSystemTempDirectory "kube-cert" $ \ tmpdir -> do
        initCA tmpdir

        withCurrentDirectory tmpdir $ do

            sans <- defaultSans opts
        
            r <- runEitherT $ do
                setupCA
                setupServerCert sans
                setupKubeletCert
                setupKubecfgCert

            case r of
                Right () -> putStrLn "Certs generated successfully "
                Left err -> putStrLn $ "Failed to generate certs: " <> err

            let certFiles = 
                    [
                    "certs/ca.crt"
                    , "private/ca.key"
                    , "certs/server.crt"
                    , "certs/kubelet.crt"
                    , "certs/kubecfg.crt"
                    , "private/server.key"
                    , "private/kubelet.key"
                    , "private/kubecfg.key"
                    ]

            forM_ certFiles $ \ certFile ->
                copyFile certFile (optsCertDir opts </> takeFileName certFile)
            

            gid <- groupID <$> getGroupEntryForName (optsCertGroup opts)

            forM_ certFiles $ \ certFile ->
                setOwnerAndGroup 
                    (optsCertDir opts </> takeFileName certFile) 
                    (-1)
                    gid

            let perms =
                    foldr
                        unionFileModes
                        nullFileMode
                        [ ownerReadMode
                        , ownerWriteMode
                        , groupReadMode
                        , groupWriteMode]

            forM_ certFiles $ \ certFile ->
                setFileMode
                    (optsCertDir opts </> takeFileName certFile)
                    perms

            -- mapM_ putStrLn =<< 
            --     (lines . Text.unpack . snd <$> sysOut "openssl" ["x509", "-noout", "-text", "-in", "certs/kubelet.crt"])

            -- mapM_ putStrLn =<< 
            --     (lines . Text.unpack . snd <$> sysOut "openssl" ["x509", "-noout", "-text", "-in", "certs/server.crt"])

            -- mapM_ putStrLn =<<  
            --     (lines . Text.unpack . snd <$> sysOut "openssl" ["x509", "-noout", "-text", "-in", "certs/kubecfg.crt"])

            return ()

main :: IO ()
main = do
    opts <- execParser (info (helper <*> optsParser) (fullDesc <> mempty))
    generateCerts opts
    return ()
