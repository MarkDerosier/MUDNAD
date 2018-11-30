{-# LANGUAGE OverloadedStrings  #-}

module Storage
    (
      login 
    , userFormat
    , getSaltAndHash
    , getBanner
    , useridFormat
    , redisLoadScripts
    ) where

import qualified Data.ByteString    as BS
import qualified Data.Text          as T
import qualified Data.Text.Encoding as T

import qualified Database.Redis     as RS
import qualified Crypto.Argon2      as ARG
import Crypto.Random.DRBG

import Data.Foldable
import Control.Monad.Trans.Maybe
import Control.Applicative
import Control.Monad

import Datatypes


filesToLoad :: [String]
filesToLoad = [ "../luascripts/adduser.lua" ]
       
redisLoadScripts :: RS.Connection -> [FilePath] -> IO [(FilePath, Either RS.Reply BS.ByteString)]
redisLoadScripts conn files = do
    filelist <- sequence $ fmap BS.readFile files
    zip files <$> RS.runRedis conn (sequence $ fmap RS.scriptLoad filelist)


addUser :: RS.Connection -> T.Text -> T.Text -> IO (Maybe Integer)
addUser conn username password = do
    gen <- newGenIO :: IO CtrDRBG
    let Right (salt, _) = genBytes fineSalt gen
    let user   = userFormat username
    let pass = T.encodeUtf8 password
    let Right hashedpassword = ARG.hash ARG.defaultHashOptions pass salt
    result <- RS.runRedis conn $ 
        RS.evalsha luaAddUser [user, salt, hashedpassword] [] :: IO (Either RS.Reply [Integer])
    case result of
        Right [x] -> return $ Just x
        _         -> empty
--The lua script should only ever return at most 1 number, but to make GHC happy

    where fineSalt = 16 -- http://argon2-cffi.readthedocs.io/en/stable/parameters.html
          luaAddUser = "7b4b7a334b2592d1d88fc59ce2b43ba6b39c4835" -- sha1sum of adduser.lua



login :: RS.Connection -> T.Text -> T.Text -> IO (Maybe PlayerDatabaseID)
login conn username password = runMaybeT $ do
    let pass = T.encodeUtf8 password
    uuid <- getID conn (userFormat username)
    [(_,salt), (_,hash)] <- getSaltAndHash conn (useridFormat uuid)
    let Right rehash = ARG.hash ARG.defaultHashOptions pass salt
    guard (rehash == hash)
    return (PlayerDatabaseID uuid)


-- Handle namespacing for keys in redis.
userFormat :: T.Text -> BS.ByteString
userFormat username = BS.concat ["user:", T.encodeUtf8 username]

useridFormat :: BS.ByteString -> BS.ByteString
useridFormat uuid = BS.concat ["userid:", uuid]

-- Redis getter functions.
getID :: RS.Connection -> BS.ByteString -> MaybeT IO BS.ByteString
getID conn user = MaybeT $ asum <$> RS.runRedis conn (RS.get user)

getSaltAndHash :: RS.Connection -> BS.ByteString -> MaybeT IO [(BS.ByteString, BS.ByteString)]
getSaltAndHash conn uuid = MaybeT $ do
    result <- asum <$> RS.runRedis conn (RS.hgetall uuid) :: IO [(BS.ByteString, BS.ByteString)]
    case result of
        [] -> empty
        _  -> return (Just result)


getBanner :: RS.Connection -> IO (Maybe BS.ByteString)
getBanner conn = asum <$> RS.runRedis conn (RS.get "server:banner")
