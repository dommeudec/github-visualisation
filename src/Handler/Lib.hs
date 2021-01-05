{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DuplicateRecordFields     #-}

module Handler.Lib
    ( someFunc
    ) where

import qualified Handler.GitHub as GH
import qualified Servant.Client               as SC
import           Network.HTTP.Client          (newManager)
import           Network.HTTP.Client.TLS      (tlsManagerSettings)
import           System.Environment           (getArgs)
import Data.Text hiding (map,intercalate, groupBy, concat)
import Data.List (intercalate, groupBy, sortBy)
import Data.Either
import           Servant.API                (BasicAuthData (..))
import Data.ByteString.UTF8 (fromString)
--import Data.Aeson.Text (encodeToLazyText)
import Data.Aeson
import Data.Aeson.Text (encodeToLazyText)
import GHC.Generics
import Data.Text.Lazy (Text)
import Data.Text.Lazy.IO as I


someFunc :: IO ()
someFunc = do
  Prelude.putStrLn "GitHubCall started.."
  (rName:user:token:_) <- getArgs
  Prelude.putStrLn $ "Name is " ++ rName
  Prelude.putStrLn $ "Github account for API call is " ++ user
  Prelude.putStrLn $ "Github token for api call is " ++ token

  let auth = BasicAuthData (fromString user) (fromString token)

  testGitHubCall auth $ pack rName
  Prelude.putStrLn ".. call finished."


testGitHubCall :: BasicAuthData -> Data.Text.Text -> IO ()
testGitHubCall auth name =

  -- Call to get user info
  (SC.runClientM (GH.getUser (Just "haskell-app") auth name) =<< env) >>= \case

  Left err -> do
    Prelude.putStrLn $ "ERROR, getting user: " ++ show err
  Right res -> do
    Prelude.putStrLn $ "User is: " ++ "\n\t" ++ show res

    -- Call to get user repositories
    (SC.runClientM (GH.getUserRepos (Just "haskell-app") auth name) =<< env) >>= \case
      Left err -> do
        Prelude.putStrLn $ "ERROR, retrieving repos: " ++ show err
      Right repos -> do
        Prelude.putStrLn $ "Repositories are:" ++ "\n\t" ++
          intercalate "\n\t" (map (\(GH.GitHubRepo n c b h) -> "[" ++ show n ++ ", " ++ show c ++ ", " ++ show b ++ ", " ++ show h ++ "]") repos)

      -- Call to get repositories' contributors, ordered by name
        partitionEithers <$> mapM (getContribs auth name) repos >>= \case

          ([], contribs) -> do
            lst <- ("[" ++ (intercalate "\n\t" .
              map (\(GH.RepoContributor n c) -> "[" ++ show n ++ "," ++ show c ++ "]") .
              groupContributors $ concat contribs) ++ "]")
            Prelude.putStrLn $ "Getting contributors: " ++ show lst

          (ers, _)-> do
            Prelude.putStrLn $ "ERROR, getting contributors: " ++ show ers

              -- Call to get repositories' commits
              --partitionEithers <$> mapM (getCommits auth name) repos >>= \case

                --([], commits) ->
                  --putStrLn $ "Commits are: " ++ "\n\t" ++ show commits
                --(ers, _)-> do
                  --putStrLn $ "ERROR, getting commits: " ++ show ers

  where env :: IO SC.ClientEnv
        env = do
          manager <- newManager tlsManagerSettings
          return $ SC.mkClientEnv manager (SC.BaseUrl SC.Http "api.github.com" 80 "")

        getContribs :: BasicAuthData -> GH.Username -> GH.GitHubRepo -> IO (Either SC.ClientError [GH.RepoContributor])
        getContribs auth name (GH.GitHubRepo repo _ _ _) =
          SC.runClientM (GH.getRepoContribs (Just "haskell-app") auth name repo) =<< env

        getCommits :: BasicAuthData -> GH.Username -> GH.GitHubRepo -> IO (Either SC.ClientError [GH.RepoCommit])
        getCommits auth name (GH.GitHubRepo repo _ _ _) =
          SC.runClientM (GH.getRepoCommit (Just "haskell-app") auth name repo) =<< env

        groupContributors :: [GH.RepoContributor] -> [GH.RepoContributor]
        groupContributors  = sortBy (\(GH.RepoContributor c1 _) (GH.RepoContributor c2 _) ->  compare c1 c2) .
                             map mapfn .
                             groupBy (\(GH.RepoContributor l1 _) (GH.RepoContributor l2 _) ->  l1 == l2)


         where mapfn :: [GH.RepoContributor] -> GH.RepoContributor
               mapfn xs@((GH.RepoContributor l _):_) = GH.RepoContributor l . sum $
                                                       map (\(GH.RepoContributor _ c) -> c)  xs
