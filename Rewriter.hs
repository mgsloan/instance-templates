{-# LANGUAGE ViewPatterns, ScopedTypeVariables #-}
module Rewriter where

import Control.Applicative ( (<$>) )
import Control.Monad       ( when, foldM, MonadPlus(..) )
import Data.List           ( partition, intersperse )
import Debug.Trace         ( trace )
import System.Directory    ( getDirectoryContents, doesFileExist, doesDirectoryExist, createDirectory, copyFile )
import System.FilePath     ( (</>), takeExtension, dropExtension, takeBaseName )
import System.Random       ( randomIO )

import Language.Haskell.Exts.Annotated
import Language.Haskell.Meta ( parseResultToEither )

-- Util from monadlist
findM :: (Monad m, MonadPlus p) => (a -> m Bool) -> [a] -> m (p a)
findM _ [] = return mzero
findM p (x:xs) = do
  bool <- p x
  if bool
    then return $ return x
    else findM p xs

partitionM :: Monad m => (a -> m Bool) -> [a] -> m ([a], [a])
partitionM p xs = foldM f ([], []) xs
 where
  f (xs, ys) x = do
    bool <- p x 
    return $ if bool then (x : xs,     ys)
                     else (    xs, x : ys)

mapSnd :: (a -> b) -> [a] -> [(a, b)]
mapSnd f = map (\x -> (x, f x))

processExample name
  = transformDir ("test-inputs" </> name) ("test-outputs" </> name) ".hs"
  $ rewriteHaskell processModule
 where
  processModule path (Module mloc header pragmas imps decls) = do
    let nameGen number = dropExtension path ++ "Class" ++ show number ++ ".hs"
        imps' = withNewPrelude imps

    when (not $ null classes) $ do
      Just (number :: Int, classFile)
        <- findM (\(_,f) -> not <$> doesFileExist f) $ mapSnd nameGen [0..]

      let header' = Just . plainHeader
                  $ maybe (takeBaseName classFile)
                          ((++ ("Class" ++ show number)) . headerName)
                          header
          classModule = Module es header' (addTH []) imps' classes
          classDerivers = concatMap doClass classes

      writeFile classFile $ prettyPrint classModule ++ "\n" ++ classDerivers
    --TODO: import class module
    return . Module mloc header (addTH pragmas) imps'
           $ doInstances insts : others
   where
    (insts, partition isClass -> (classes, others))
      = partition isInstance decls

    -- Very strange: haskell-src-exts doesn't support declaration quasi-quotes.
    doClass c@(ClassDecl _ ctx dh fds ds)
      = "[deriving'|" ++ prettyPrint classDecl ++ "\n|]\n"
     where
      -- TODO: what about the fun-deps?
      classDecl = ClassDecl es ctx dh [] $ Just [ClsDecl es instDecl]
      instDecl = InstDecl es Nothing (convertDHead dh) Nothing

  --TODO: need to represent the cxt! forgot about that.
  --TODO: what should happen to InstSigs?
  --TODO: consider qualification?
    doInstances = SpliceDecl es . foldl1 (App es) . map process
     where
      process (InstDecl _ _ _ decls)
        = App es
        ( App es
          (Var es (UnQual es $ Ident es "derive"))
          (Con es (UnQual es $ Ident es "Instance")) )
        ( QuasiQuote es "d" $ maybe "" showDecls decls )
      showDecls = concat . intersperse "\n" . map prettyPrint


-- $(derive (Instance :: Instance (Foo Int)) [])

withNewPrelude imps
  | any isPrelude imps = map (modImportName ("New"++) `modWhen` isPrelude) imps
  | otherwise = plainImport "NewPrelude" : imps
 where
  isPrelude = (== "Prelude") . getImportName

-- A poor man's lenses
getImportName (ImportDecl _ (ModuleName _ n) _ _ _ _ _) = n
modImportName func (ImportDecl a (ModuleName b n       ) c d e f g)
                  = ImportDecl a (ModuleName b (func n)) c d e f g

plainModule = ModuleName es

plainImport name
  = ImportDecl es (plainModule name) False False Nothing Nothing Nothing

-- I can't believe this isn't a common idiom / existing library function
modWhen :: (a -> a) -> (a -> Bool) -> a -> a
modWhen f g x = if g x then f x else x


addTH = addExtensions ["TemplateHaskell", "QuasiQuotes"]

addExtensions exts = map process
 where
  process (LanguagePragma l ns) = LanguagePragma l (map (Ident l) exts ++ ns)
  process x = x

isInstance, isClass :: Decl a -> Bool

isInstance (InstDecl _ _ _ _) = True
isInstance _                  = False

isClass (ClassDecl _ _ _ _ _) = True
isClass _                     = False

headerName (ModuleHead _ (ModuleName _ n) _ _) = n

plainHeader name = ModuleHead es (ModuleName es name) Nothing Nothing

convertDHead (DHead   _ n tvs) = IHead   es (uq n) (map tyVarType tvs)
convertDHead (DHInfix _ l n r) = IHInfix es (tyVarType l) (uq n) (tyVarType r)
convertDHead (DHParen _ x)     = IHParen es $ convertDHead x

tyVarType (KindedVar   _ n _) = TyVar es n
tyVarType (UnkindedVar _ n  ) = TyVar es n

uq = UnQual es

es = noInfoSpan $ SrcSpan "" 0 0 0 0

transformDir :: FilePath -> FilePath -> String
             -> (FilePath -> String -> IO String)
             -> IO ()
transformDir inputDir outputDir ext func = do
  createDirectory outputDir
  contents <- filter (`notElem` [".", ".."]) <$> getDirectoryContents inputDir
  (dirs, files) <- partitionM doesDirectoryExist contents

  let (hsfiles, others) = partition ((ext ==) . takeExtension) files

  -- Apply the transformation to all files with the extension
  mapM_ (\f -> writeFile (outputDir </> f)
           =<< func      (outputDir </> f)
           =<< readFile  (inputDir  </> f)) hsfiles
  
  mapM_ (\f -> copyFile  (inputDir  </> f)
                         (outputDir </> f)) others

  -- Recurse
  mapM_ (\d -> transformDir (inputDir </> d) (outputDir </> d) ext func)
        dirs

--TODO: argh! lhs?!

-- (`exactPrint` cs)

rewriteHaskell :: (FilePath -> Module SrcSpanInfo -> IO (Module SrcSpanInfo))
               -> FilePath -> String -> IO String 
rewriteHaskell func file code
  = either (\e -> trace (mkErr e) (return code))
           (\(m, cs) -> prettyPrint <$> func file m)
  . parseResultToEither
  $ parseModuleWithComments defaultParseMode code
 where
  mkErr e = "Couldn't parse file " ++ file ++ " - leaving it alone.\n" ++ e
