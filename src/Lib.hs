{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
module Lib
    ( endpoint
    ) where

import Control.Applicative
import Control.Monad.IO.Class
import Data.Aeson
import Data.Int
import Data.Proxy
import Data.Text                                 (Text)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import GHC.Generics
import Network.Wai.Handler.Warp           hiding (Connection)
import Servant

data Book = Book { title  :: Text
                 , author :: Text
                 } deriving Generic

instance FromJSON Book
instance ToJSON Book

instance FromRow Book where
  fromRow = Book <$> field <*> field

instance ToRow Book where
  toRow book = [ toField (title book)
               , toField (author book)]

type BookApi = "books" :> ReqBody '[JSON] Book :> Post '[JSON] Book
          :<|> "books" :> Get '[JSON] [Book]

insertBook :: Connection -> Book -> IO Int64
insertBook conn =
  execute conn "insert into books values (?, ?)"

selectAllBooks :: Connection -> IO [Book]
selectAllBooks conn =
  query_ conn "select * from books"

server :: Connection -> Server BookApi
server conn = postBook
         :<|> getBooks
  where
    postBook book = liftIO $ insertBook conn book >> return book
    getBooks      = liftIO $ selectAllBooks conn

bookApi :: Proxy BookApi
bookApi = Proxy

endpoint :: IO ()
endpoint = do
  conn <- connectPostgreSQL "dbname=booksdb"
  run 8080 (serve bookApi $ server conn)
