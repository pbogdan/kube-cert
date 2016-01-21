module Lib.IP
    (retrieveIp
    , parseMasterIP
    , firstIpInRange
    ) where

import           Control.Lens ((&),(.~),(^.))
import           Control.Monad.IO.Class (MonadIO,liftIO)
import qualified Data.ByteString.Lazy.Char8 as ByteChar
import           Data.IP (AddrRange, IPv4,addr)
import           Network.Wreq (defaults, get, getWith, header, responseBody)

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
