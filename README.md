# bcc-sl-x509

> Pure Haskell 'replacement' for OpenSSL to generate certificates for a TLS Private Key Infrastructure

## How to Use

```hs
import           Crypto.PubKey.RSA (PrivateKey, PublicKey)

import           Bcc.X509.Configuration (DirConfiguration(..), CertDescription(..),
                     decodeConfigFile, fromConfiguration, genCertificate)
import           Data.X509.Extra (genRSA256KeyPair)


main :: IO ()
main = do
    confFile <-
        decodeConfigFile "dev" "lib/configuration.yaml"

    let dirConf =
          DirConfiguration "server" "client" Nothing

    (caDesc, certDescs) <-
        fromConfiguration confFile dirConf genRSA256KeyPair <$> genRSA256KeyPair

    (caKey, caCert) <- 
        genCertificate caDesc

    (clientKey, clientCert) <-
        genCertificate (findCert "client" certDescs)

    (serverKey, serverCert) <-
        genCertificate (findCert "server" certDescs)

    -- Do something with the Private Key Infrastructure
  where
    findCert 
      :: String 
      -> [CertDescription IO PublicKey PrivateKey String]
      -> CertDescription IO PublicKey PrivateKey String
    findCert outDir =
        head . find ((== outDir) . certOutDir)