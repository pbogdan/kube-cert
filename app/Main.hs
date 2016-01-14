module Main where

import Options.Applicative

import Lib

import Data.Word (Word8)
import Data.IP

data Opts = Opts
    { optMasterIp              :: !String
    , optMasterName            :: !String
    , optDnsDomain             :: !String
    , optServiceClusterIpRange :: !String
    , optCertDir               :: !String
    , optCertGroup             :: !String
    }


parseOpts :: Parser Opts
parseOpts =
    Opts <$>
    option
        auto
        (long "master-ip" <> showDefault <> metavar "master-ip" <>
         help "master ip") <*>
    option
        auto
        (long "master-name" <> showDefault <> metavar "master-name" <>
         help "master name" <>
         value "kubernetes") <*>
    option
        auto
        (long "dns-domain" <> showDefault <> metavar "dns-domain" <>
         help "dns domain" <>
         value "cluster.local") <*>
    option
        auto
        (long "service-cluster-ip-range" <> showDefault <>
         metavar "service-cluster-ip-range" <>
         help "service cluster-ip-range" <>
         value "10.0.0.0/16") <*>
    option
        auto
        (long "cert-dir" <> showDefault <> metavar "cert-dir" <>
         help "cert dir" <>
         value "/srv/kubernetes") <*>
    option
        auto
        (long "cert-group" <> showDefault <> metavar "cert-group" <>
         help "cert group" <>
         value "kube-cert")

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

retrieveIp :: (Monad m) => MasterIp -> m IPv4
retrieveIp ip = case ip of
    IpAddress address -> parseIp address
    GceExternalIp     -> fetchGceIp
    AwsExternalIp     -> fetchAwsIp
    AzureDnsName      -> fetchAzureIp

parseIp :: (Monad m) => String -> m IPv4
parseIp s = return (read s :: IPv4)

fetchGceIp :: (Monad m) => m IPv4
fetchGceIp = undefined
    
fetchAwsIp :: (Monad m) => m IPv4
fetchAwsIp = undefined

fetchAzureIp :: (Monad m) => m IPv4
fetchAzureIp = undefined

main :: IO ()
main = do
    opts <- execParser (info (helper <*> parseOpts) (fullDesc <> mempty))
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
