module Lib.Opts
    ( Opts(..)
    , optsParser
    , defaultOpts
    ) where

import Options.Applicative

data Opts = Opts
    { optsMasterIp              :: !String
    , optsMasterName            :: !String
    , optsDnsDomain             :: !String
    , optsServiceClusterIpRange :: !String
    , optsCertDir               :: !String
    , optsCertGroup             :: !String
    } deriving (Show)

optsParser :: Parser Opts
optsParser =
    Opts <$>
    option
        str
        (long "master-ip" <> showDefault <> metavar "master-ip" <>
         help "master ip") <*>
    option
        str
        (long "master-name" <> showDefault <> metavar "master-name" <>
         help "master name" <>
         value "kubernetes") <*>
    option
        str
        (long "dns-domain" <> showDefault <> metavar "dns-domain" <>
         help "dns domain" <>
         value "cluster.local") <*>
    option
        str
        (long "service-cluster-ip-range" <> showDefault <>
         metavar "service-cluster-ip-range" <>
         help "service cluster-ip-range" <>
         value "10.0.0.0/16") <*>
    option
        str
        (long "cert-dir" <> showDefault <> metavar "cert-dir" <>
         help "cert dir" <>
         value "/srv/kubernetes") <*>
    option
        str
        (long "cert-group" <> showDefault <> metavar "cert-group" <>
         help "cert group" <>
         value "kube-cert")

defaultOpts :: Opts
defaultOpts =
    Opts
    { optsMasterIp = "192.168.1.10"
    , optsMasterName = "kubernetes.master"
    , optsDnsDomain = "kubernetes.cluster"
    , optsServiceClusterIpRange = "10.0.0.0/16"
    , optsCertDir = "/tmp"
    , optsCertGroup = "piotr"
    }
