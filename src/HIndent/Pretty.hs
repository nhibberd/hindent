{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Pretty printing.

module HIndent.Pretty
  (pretty)
  where

import           Control.Applicative
import           Control.Monad.State.Strict hiding (state)
import qualified Data.ByteString.Builder as S
import           Data.Foldable (for_, traverse_)
import           Data.Int
import           Data.List
import           Data.Maybe
import           Data.Monoid ((<>))
import           Data.Typeable
import           HIndent.Types
import qualified Language.Haskell.Exts as P
import           Language.Haskell.Exts.SrcLoc
import           Language.Haskell.Exts.Syntax
import           Prelude hiding (exp)

--------------------------------------------------------------------------------
-- * Pretty printing class

-- | Pretty printing class.
class (Annotated ast,Typeable ast) => Pretty ast where
  prettyInternal :: ast NodeInfo -> Printer ()

-- | Pretty print including comments.
pretty :: (Pretty ast,Show (ast NodeInfo))
       => ast NodeInfo -> Printer ()
pretty a = do
  mapM_
    (\c' -> do
       case c' of
         CommentBeforeLine c -> do
           case c of
             EndOfLine s -> write ("--" ++ s)
             MultiLine s -> write ("{-" ++ s ++ "-}")
           newline
         _ -> return ())
    comments
  prettyInternal a
  mapM_
    (\(i, c') -> do
       case c' of
         CommentSameLine c -> do
           col <- gets psColumn
           unless (col == 0) space
           writeComment c
         CommentAfterLine c -> do
           when (i == 0) newline
           writeComment c
         _ -> return ())
    (zip [0 :: Int ..] comments)
  where
    comments = nodeInfoComments (ann a)
    writeComment =
      \case
        EndOfLine cs -> do
          write ("--" ++ cs)
          modify
            (\s ->
                s
                { psEolComment = True
                })
        MultiLine cs -> do
          write ("{-" ++ cs ++ "-}")
          modify
            (\s ->
                s
                { psEolComment = True
                })

-- | Pretty print using HSE's own printer. The 'P.Pretty' class here
-- is HSE's.
pretty' :: (Pretty ast,P.Pretty (ast SrcSpanInfo))
        => ast NodeInfo -> Printer ()
pretty' = write . P.prettyPrint . fmap nodeInfoSpan

--------------------------------------------------------------------------------
-- * Combinators

-- | Increase indentation level by n spaces for the given printer.
indented :: Int64 -> Printer a -> Printer a
indented i p =
  do level <- gets psIndentLevel
     modify (\s -> s {psIndentLevel = level + i})
     m <- p
     modify (\s -> s {psIndentLevel = level})
     return m

indentedBlock :: Printer a -> Printer a
indentedBlock p =
  do indentSpaces <- getIndentSpaces
     indented indentSpaces p

-- | Print all the printers separated by spaces.
spaced :: [Printer ()] -> Printer ()
spaced = inter space

spacedWrap :: [Printer ()] -> Printer ()
spacedWrap p =
  ifFitsOnOneLineOrElse (spaced p) (lined p)

-- | Print all the printers separated by commas.
commas :: [Printer ()] -> Printer ()
commas = inter (write ", ")

-- | Print all the printers separated by sep.
inter :: Printer () -> [Printer ()] -> Printer ()
inter sep ps =
  foldr
    (\(i,p) next ->
        depend
          (do p
              if i < length ps
                then sep
                else return ())
          next)
    (return ())
    (zip [1 ..] ps)

-- | Print all the printers separated by newlines.
lined :: [Printer ()] -> Printer ()
lined ps = sequence_ (intersperse newline ps)

-- | Print all the printers separated newlines and optionally a line
-- prefix.
prefixedLined :: String -> [Printer ()] -> Printer ()
prefixedLined pref ps' =
  case ps' of
    [] -> return ()
    (p:ps) ->
      do p
         indented (fromIntegral
                     (length pref *
                      (-1)))
                  (mapM_ (\p' ->
                            do newline
                               depend (write pref) p')
                         ps)

-- | Set the (newline-) indent level to the given column for the given
-- printer.
column :: Int64 -> Printer a -> Printer a
column i p =
  do level <- gets psIndentLevel
     modify (\s -> s {psIndentLevel = i})
     m <- p
     modify (\s -> s {psIndentLevel = level})
     return m

-- | Output a newline.
newline :: Printer ()
newline =
  do write "\n"
     modify (\s -> s {psNewline = True})

-- | Set the context to a case context, where RHS is printed with -> .
withCaseContext :: Bool -> Printer a -> Printer a
withCaseContext bool pr =
  do original <- gets psInsideCase
     modify (\s -> s {psInsideCase = bool})
     result <- pr
     modify (\s -> s {psInsideCase = original})
     return result

-- | Get the current RHS separator, either = or -> .
rhsSeparator :: Printer ()
rhsSeparator =
  do inCase <- gets psInsideCase
     if inCase
        then write "->"
        else write "="

-- | Make the latter's indentation depend upon the end column of the
-- former.
depend :: Printer () -> Printer b -> Printer b
depend maker dependent =
  do state' <- get
     maker
     st <- get
     col <- gets psColumn
     if psLine state' /= psLine st || psColumn state' /= psColumn st
        then column col dependent
        else dependent

-- | Wrap.
wrap :: String -> String -> Printer a -> Printer a
wrap open close p = depend (write open) $ p <* write close

-- | Wrap in parens.
parens :: Printer a -> Printer a
parens = wrap "(" ")"

-- | Wrap in braces.
braces :: Printer a -> Printer a
braces = wrap "{" "}"

-- | Wrap in brackets.
brackets :: Printer a -> Printer a
brackets = wrap "[" "]"

-- | Write a space.
space :: Printer ()
space = write " "

-- | Write a comma.
comma :: Printer ()
comma = write ","

-- | Write an integral.
int :: Integer -> Printer ()
int = write . show

-- | Write out a string, updating the current position information.
write :: String -> Printer ()
write x =
  do eol <- gets psEolComment
     hardFail <- gets psHardLimit
     let addingNewline = eol && x /= "\n"
     when addingNewline newline
     state <- get
     let writingNewline = x == "\n"
         out :: String
         out =
           if psNewline state && not writingNewline
              then (replicate (fromIntegral (psIndentLevel state))
                               ' ') <>
                   x
              else x
         psColumn' =
            if additionalLines > 0
               then fromIntegral (length (concat (take 1 (reverse srclines))))
               else psColumn state + fromIntegral (length out)
     when
       hardFail
       (guard
          (additionalLines == 0 &&
           (psColumn' <= configMaxColumns (psConfig state))))
     modify (\s ->
               s {psOutput = psOutput state <> S.stringUtf8 out
                 ,psNewline = False
                 ,psLine = psLine state + fromIntegral additionalLines
                 ,psEolComment= False
                 ,psColumn = psColumn'})
  where srclines = lines x
        additionalLines =
          length (filter (== '\n') x)

-- | Write a string.
string :: String -> Printer ()
string = write

-- | Indent spaces, e.g. 2.
getIndentSpaces :: Printer Int64
getIndentSpaces =
  gets (configIndentSpaces . psConfig)

-- | Play with a printer and then restore the state to what it was
-- before.
sandbox :: Printer a -> Printer (a,PrintState)
sandbox p =
  do orig <- get
     a <- p
     new <- get
     put orig
     return (a,new)

-- | Render a type with a context, or not.
withCtx :: (Pretty ast,Show (ast NodeInfo))
        => Maybe (ast NodeInfo) -> Printer b -> Printer b
withCtx Nothing m = m
withCtx (Just ctx) m =
  do pretty ctx
     write " =>"
     newline
     m

-- | Maybe render an overlap definition.
maybeOverlap ::  Maybe (Overlap NodeInfo) -> Printer ()
maybeOverlap =
  maybe (return ())
        (\p ->
           pretty p >>
           space)

-- | Swing the second printer below and indented with respect to the first.
swing :: Printer () -> Printer b -> Printer ()
swing a b =
  do orig <- gets psIndentLevel
     a
     mst <- fitsOnOneLine (do space
                              b)
     case mst of
       Just st -> put st
       Nothing -> do newline
                     indentSpaces <- getIndentSpaces
                     _ <- column (orig + indentSpaces) b
                     return ()

-- | Swing the second printer below and indented with respect to the first by
-- the specified amount.
swingBy :: Int64 -> Printer() -> Printer b -> Printer b
swingBy i a b =
  do orig <- gets psIndentLevel
     a
     newline
     column (orig + i) b

--------------------------------------------------------------------------------
-- * Instances

instance Pretty Context where
  prettyInternal ctx@(CxTuple _ asserts) = do
    mst <- fitsOnOneLine (parens (inter (comma >> space) (map pretty asserts)))
    case mst of
      Nothing -> context ctx
      Just st -> put st
  prettyInternal ctx = context ctx

instance Pretty Pat where
  prettyInternal x =
    case x of
      PLit _ sign l -> pretty sign >> pretty l
      PNPlusK _ n k ->
        depend (do pretty n
                   write "+")
               (int k)
      PInfixApp _ a op b ->
        case op of
          Special{} ->
            depend (pretty a)
                   (depend (prettyInfixOp op)
                           (pretty b))
          _ ->
            depend (do pretty a
                       space)
                   (depend (do prettyInfixOp op
                               space)
                           (pretty b))
      PApp _ f args ->
        depend (do pretty f
                   unless (null args) space)
               (spaced (map pretty args))
      PTuple _ boxed pats ->
        depend (write (case boxed of
                         Unboxed -> "(#"
                         Boxed -> "("))
               (do commas (map pretty pats)
                   write (case boxed of
                            Unboxed -> "#)"
                            Boxed -> ")"))
      PList _ ps ->
        brackets (commas (map pretty ps))
      PParen _ e -> parens (pretty e)
      PRec _ qname fields -> do
        let horVariant = do
              pretty qname
              space
              braces $ commas $ map pretty fields
            verVariant =
              depend (pretty qname >> space) $ do
                case fields of
                  [] -> write "{}"
                  [field] -> braces $ pretty field
                  _ -> do
                    depend (write "{") $
                      prefixedLined "," $ map (depend space . pretty) fields
                    newline
                    write "}"
        horVariant `ifFitsOnOneLineOrElse` verVariant
      PAsPat _ n p ->
        depend (do pretty n
                   write "@")
               (pretty p)
      PWildCard _ -> write "_"
      PIrrPat _ p ->
        depend (write "~")
               (pretty p)
      PatTypeSig _ p ty ->
        depend (do pretty p
                   write " :: ")
               (pretty ty)
      PViewPat _ e p ->
        depend (do pretty e
                   write " -> ")
               (pretty p)
      PQuasiQuote _ name str ->
        brackets (depend (do write "$"
                             string name
                             write "|")
                         (string str))
      PBangPat _ p ->
        depend (write "!")
               (pretty p)
      PRPat{} -> pretty' x
      PXTag{} -> pretty' x
      PXETag{} -> pretty' x
      PXPcdata{} -> pretty' x
      PXPatTag{} -> pretty' x
      PXRPats{} -> pretty' x
      PVar{} -> pretty' x

-- | Pretty print a name for being an infix operator.
prettyInfixOp ::  QName NodeInfo -> Printer ()
prettyInfixOp x =
  case x of
    Qual _ mn n ->
      case n of
        Ident _ i -> do write "`"; pretty mn; write "."; string i; write "`";
        Symbol _ s -> do pretty mn; write "."; string s;
    UnQual _ n ->
      case n of
        Ident _ i -> string ("`" ++ i ++ "`")
        Symbol _ s -> string s
    Special _ s -> pretty s

instance Pretty Type where
  prettyInternal  =
    typ

instance Pretty Exp where
  prettyInternal = exp

-- | Render an expression.
exp :: Exp NodeInfo -> Printer ()
-- | Do after lambda should swing.
exp (Lambda _ pats (Do l stmts)) =
  do
     mst <-
          fitsOnOneLine
            (do write "\\"
                spaced (map pretty pats)
                write " -> "
                pretty (Do l stmts))
     case mst of
       Nothing -> swing (do write "\\"
                            spaced (map pretty pats)
                            write " -> do")
                         (lined (map pretty stmts))
       Just st -> put st
-- | Space out tuples.
exp (Tuple _ boxed exps) = do
  let horVariant = parensB boxed $ inter (write ", ") (map pretty exps)
      verVariant = parensB boxed $ prefixedLined "," (map (depend space . pretty) exps)
  mst <- fitsOnOneLine horVariant
  case mst of
    Nothing -> verVariant
    Just st -> put st
  where
    parensB Unboxed = wrap "(#" "#)"
    parensB Boxed   = parens
-- | Space out tuples.
exp (TupleSection _ boxed mexps) = do
  let horVariant = parensB boxed $ inter (write ", ") (map (maybe (return ()) pretty) mexps)
      verVariant =
        parensB boxed $ prefixedLined "," (map (maybe (return ()) (depend space . pretty)) mexps)
  mst <- fitsOnOneLine horVariant
  case mst of
    Nothing -> verVariant
    Just st -> put st
  where
    parensB Unboxed = wrap "(#" "#)"
    parensB Boxed   = parens
-- | Infix apps, same algorithm as ChrisDone at the moment.
exp e@(InfixApp _ a op b) =
  infixApp e a op b Nothing
-- | If bodies are indented 4 spaces. Handle also do-notation.
exp (If _ if' then' else') = do
  let oneline = do
        depend (write "if ") (pretty if')
        depend (write " then ") (pretty then')
        depend (write " else ") (pretty else')
      multiline = do
        depend (write "if ") (pretty if')
        branch " then" then'
        newline
        branch "else" else'
  mol <- fitsOnOneLine oneline
  maybe multiline put mol

     -- Special handling for do.
  where
      branch str e =
        case e of
            Do _ stmts ->
              do write str
                 write " do"
                 newline
                 indentSpaces <- getIndentSpaces
                 indented indentSpaces (lined (map pretty stmts))
            _ -> do
              write str
              newline
              indentSpaces <- getIndentSpaces
              indented indentSpaces (pretty e)

-- | Render on one line, or otherwise render the op with the arguments
-- listed line by line.
exp (App _ op arg) = do
  let flattened = flatten op ++ [arg]
  mst <- fitsOnOneLine (spaced (map pretty flattened))
  case mst of
    Nothing -> do
      let (f:args) = flattened
      pretty f
      newline
      spaces <- getIndentSpaces
      indented spaces (lined (map pretty args))
    Just st -> put st
  where
    flatten (App label' op' arg') = flatten op' ++ [amap (addComments label') arg']
    flatten x = [x]
    addComments n1 n2 =
      n2
      { nodeInfoComments = nub (nodeInfoComments n2 ++ nodeInfoComments n1)
      }
-- | Space out commas in list.
exp (List _ es) =
  do mst <- fitsOnOneLine p
     case mst of
       Nothing -> do
         depend
           (write "[")
           (prefixedLined "," (map (depend space . pretty) es))
         newline
         write "]"
       Just st -> put st
  where p =
          brackets (inter (write ", ")
                          (map pretty es))
exp (RecUpdate _ exp' updates) = recUpdateExpr (pretty exp') updates
exp (RecConstr _ qname updates) = recUpdateExpr (pretty qname) updates
exp (Let _ binds e) =
  depend (write "let ")
         (do pretty binds
             newline
             indented (-4) (depend (write "in ")
                                   (pretty e)))
exp (ListComp _ e qstmt) = do
  let horVariant = brackets $ do
        pretty e
        write " | "
        commas $ map pretty qstmt
      verVariant = do
        write "[ "
        pretty e
        newline
        depend (write "| ") $ prefixedLined ", " $ map pretty qstmt
        newline
        write "]"
  horVariant `ifFitsOnOneLineOrElse` verVariant

exp (ParComp _ e qstmts) = do
  let horVariant = brackets $ do
        pretty e
        for_ qstmts $ \qstmt -> do
          write " | "
          commas $ map pretty qstmt
      verVariant = do
        depend (write "[ ") $ pretty e
        newline
        for_ qstmts $ \qstmt -> do
          depend (write "| ") $ prefixedLined ", " $ map pretty qstmt
          newline
        write "]"
  horVariant `ifFitsOnOneLineOrElse` verVariant

exp (TypeApp _ t) = do
  write "@"
  pretty t

exp (ExprHole {}) = write "_"
exp (NegApp _ e) =
  depend (write "-")
         (pretty e)
exp (Lambda _ ps e) = do
  write "\\"
  spaced [ do case (i, x) of
                (0, PIrrPat {}) -> space
                (0, PBangPat {}) -> space
                _ -> return ()
              pretty x
         | (i, x) <- zip [0 :: Int ..] ps
         ]
  swing (write " ->") $ pretty e
exp (Paren _ e) = parens (pretty e)
exp (Case _ e alts) =
  do depend (write "case ")
            (do pretty e
                write " of")
     newline
     indentedBlock (lined (map (withCaseContext True . pretty) alts))
exp (Do _ stmts) =
  depend (write "do ")
         (lined (map pretty stmts))
exp (MDo _ stmts) =
  depend (write "mdo ")
         (lined (map pretty stmts))
exp (LeftSection _ e op) =
  parens (depend (do pretty e
                     space)
                 (pretty op))
exp (RightSection _ e op) =
  parens (depend (do pretty e
                     space)
                 (pretty op))
exp (EnumFrom _ e) =
  brackets (do pretty e
               write " ..")
exp (EnumFromTo _ e f) =
  brackets (depend (do pretty e
                       write " .. ")
                   (pretty f))
exp (EnumFromThen _ e t) =
  brackets (depend (do pretty e
                       write ",")
                   (do pretty t
                       write " .."))
exp (EnumFromThenTo _ e t f) =
  brackets (depend (do pretty e
                       write ",")
                   (depend (do pretty t
                               write " .. ")
                           (pretty f)))
exp (ExpTypeSig _ e t) =
  depend (do pretty e
             write " :: ")
         (pretty t)
exp (VarQuote _ x) =
  depend (write "'")
         (pretty x)
exp (TypQuote _ x) =
  depend (write "''")
         (pretty x)
exp (BracketExp _ b) = pretty b
exp (SpliceExp _ s) = pretty s
exp (QuasiQuote _ n s) =
  brackets (depend (do string n
                       write "|")
                   (do string s
                       write "|"))
exp (LCase _ alts) =
  do write "\\case"
     newline
     indentedBlock (lined (map (withCaseContext True . pretty) alts))
exp (MultiIf _ alts) =
  withCaseContext
    True
    (depend
       (write "if ")
       (lined
          (map
             (\p -> do
                write "| "
                prettyG p)
             alts)))
  where
    prettyG (GuardedRhs _ stmts e) = do
      indented
        1
        (do (lined (map
                         (\(i,p) -> do
                            unless (i == 1)
                                   space
                            pretty p
                            unless (i == length stmts)
                                   (write ","))
                         (zip [1..] stmts))))
      swing (write " " >> rhsSeparator) (pretty e)
exp (Lit _ lit) = prettyInternal lit
exp (Var _ q) = case q of
                  Special _ Cons{} -> parens (pretty q)
                  Qual _ _ (Symbol _ _) -> parens (pretty q)
                  UnQual _ (Symbol _ _) -> parens (pretty q)
                  _ -> pretty q
exp (IPVar _ q) = pretty q
exp (Con _ q) = case q of
                  Special _ Cons{} -> parens (pretty q)
                  _ -> pretty q

exp x@XTag{} = pretty' x
exp x@XETag{} = pretty' x
exp x@XPcdata{} = pretty' x
exp x@XExpTag{} = pretty' x
exp x@XChildTag{} = pretty' x
exp x@CorePragma{} = pretty' x
exp x@SCCPragma{} = pretty' x
exp x@GenPragma{} = pretty' x
exp x@Proc{} = pretty' x
exp x@LeftArrApp{} = pretty' x
exp x@RightArrApp{} = pretty' x
exp x@LeftArrHighApp{} = pretty' x
exp x@RightArrHighApp{} = pretty' x
exp x@ParArray{} = pretty' x
exp x@ParArrayFromTo{} = pretty' x
exp x@ParArrayFromThenTo{} = pretty' x
exp x@ParArrayComp{} = pretty' x
exp (OverloadedLabel _ label) = string ('#' : label)

instance Pretty IPName where
 prettyInternal = pretty'

instance Pretty Stmt where
  prettyInternal =
    stmt

instance Pretty QualStmt where
  prettyInternal x =
    case x of
      QualStmt _ s -> pretty s
      ThenTrans _ s -> do
        write "then "
        pretty s
      ThenBy _ s t -> do
        write "then "
        pretty s
        write " by "
        pretty t
      GroupBy _ s -> do
        write "then group by "
        pretty s
      GroupUsing _ s -> do
        write "then group using "
        pretty s
      GroupByUsing _ s t -> do
        write "then group by "
        pretty s
        write " using "
        pretty t

instance Pretty Decl where
  prettyInternal = decl'

-- | Render a declaration.
declx :: Decl NodeInfo -> Printer ()
declx (PatBind _ pat rhs' mbinds) =
  do pretty pat
     withCaseContext False (pretty rhs')
     case mbinds of
       Nothing -> return ()
       Just binds ->
         do newline
            indentedBlock (depend (write "where ")
                                  (pretty binds))
declx (InstDecl _ moverlap dhead decls) =
  do depend (write "instance ")
            (depend (maybeOverlap moverlap)
                    (depend (pretty dhead)
                            (unless (null (fromMaybe [] decls))
                                    (write " where"))))
     unless (null (fromMaybe [] decls))
            (do newline
                indentedBlock (lined (map pretty (fromMaybe [] decls))))
declx (SpliceDecl _ e) = pretty e
declx (TypeSig _ names ty) =
  {- N.B. This is never called. See decl' -}
  swing (do inter (write ", ") (map pretty names)
            write " :: ")
         (pretty ty)
declx (FunBind _ matches) =
  lined (map pretty matches)
declx (ClassDecl _ ctx dhead fundeps decls) =
  do depend (write "class ")
            (withCtx ctx
                     (depend (do pretty dhead)
                             (depend (unless (null fundeps)
                                             (do write " | "
                                                 commas (map pretty fundeps)))
                                     (unless (null (fromMaybe [] decls))
                                             (write " where")))))
     unless (null (fromMaybe [] decls))
            (do newline
                indentedBlock (lined (map pretty (fromMaybe [] decls))))
declx (TypeDecl _ typehead typ') =
  depend (write "type ")
         (depend (pretty typehead)
                 (depend (write " = ")
                         (pretty typ')))

declx (TypeFamDecl _ declhead result injectivity) = do
  write "type family "
  pretty declhead
  case result of
    Just r -> do
      space
      write "="
      space
      pretty r
    Nothing -> return ()
  case injectivity of
    Just i -> do
      space
      pretty i
    Nothing -> return ()
declx (DataDecl _ dataornew ctx dhead condecls mderivs) =
  do depend (do pretty dataornew
                space)
            (withCtx ctx
                     (do pretty dhead
                         case condecls of
                           [] -> return ()
                           [x] -> singleCons x
                           xs -> multiCons xs))
     indentSpaces <- getIndentSpaces
     case mderivs of
       Nothing -> return ()
       Just derivs ->
         do newline
            column indentSpaces (pretty derivs)
  where singleCons x =
          do write " ="
             indentSpaces <- getIndentSpaces
             column indentSpaces
                    (do newline
                        pretty x)
        multiCons xs =
          do newline
             indentSpaces <- getIndentSpaces
             column indentSpaces
                    (depend (write "=")
                            (prefixedLined "|"
                                           (map (depend space . pretty) xs)))

declx (InlineSig _ inline active name) = do
  write "{-# "

  unless inline $ write "NO"
  write "INLINE "
  case active of
    Nothing -> return ()
    Just (ActiveFrom _ x) -> write ("[" ++ show x ++ "] ")
    Just (ActiveUntil _ x) -> write ("[~" ++ show x ++ "] ")
  pretty name

  write " #-}"
declx x' = pretty' x'

instance Pretty Deriving where
  prettyInternal (Deriving _ heads) =
    do write "deriving"
       space
       let heads' =
             if length heads == 1
                then map stripParens heads
                else heads
       parens (commas (map pretty heads'))
    where stripParens (IParen _ iRule) = stripParens iRule
          stripParens x = x

instance Pretty Alt where
  prettyInternal x =
    case x of
      Alt _ p galts mbinds ->
        do pretty p
           pretty galts
           case mbinds of
             Nothing -> return ()
             Just binds ->
               do newline
                  indentedBlock (depend (write "where ")
                                (pretty binds))

instance Pretty Asst where
  prettyInternal x =
    case x of
      ClassA _ name types -> spaced (pretty name : map pretty types)
      i@InfixA {} -> pretty' i
      IParam _ name ty -> do
        pretty name
        write " :: "
        pretty ty
      EqualP _ a b -> do
        pretty a
        write " ~ "
        pretty b
      ParenA _ asst -> parens (pretty asst)
      AppA _ name tys -> spaced (pretty name : map pretty (reverse tys))
      WildCardA _ name ->
        case name of
          Nothing -> write "_"
          Just n -> do
            write "_"
            pretty n

instance Pretty BangType where
  prettyInternal x =
    case x of
      BangedTy _ -> write "!"
      LazyTy _ -> write "~"
      NoStrictAnnot _ -> return ()

instance Pretty Unpackedness where
  prettyInternal (Unpack _) = write "{-# UNPACK #-}"
  prettyInternal (NoUnpack _) = write "{-# NOUNPACK #-}"
  prettyInternal (NoUnpackPragma _) = return ()

instance Pretty Binds where
  prettyInternal x =
    case x of
      BDecls _ ds -> lined (map pretty ds)
      IPBinds _ i -> lined (map pretty i)

instance Pretty ClassDecl where
  prettyInternal x =
    case x of
      ClsDecl _ d -> pretty d
      ClsDataFam _ ctx h mkind ->
        depend (write "data ")
               (withCtx ctx
                        (do pretty h
                            (case mkind of
                               Nothing -> return ()
                               Just kind ->
                                 do write " :: "
                                    pretty kind)))
      ClsTyFam _ h mkind minj ->
        depend (write "type ")
               (depend (pretty h)
                       (depend (traverse_ (\kind -> write " :: " >> pretty kind) mkind)
                               (traverse_ pretty minj)))
      ClsTyDef _ (TypeEqn _ this that) ->
        do write "type "
           pretty this
           write " = "
           pretty that
      ClsDefSig _ name ty ->
        do write "default "
           pretty name
           write " :: "
           pretty ty

instance Pretty ConDecl where
  prettyInternal x =
    conDecl x

instance Pretty FieldDecl where
  prettyInternal (FieldDecl _ names ty) =
    depend (do commas (map pretty names)
               write " :: ")
           (pretty ty)

instance Pretty FieldUpdate where
  prettyInternal x =
    case x of
      FieldUpdate _ n e ->
        swing (do pretty n
                  write " =")
               (pretty e)
      FieldPun _ n -> pretty n
      FieldWildcard _ -> write ".."

instance Pretty GuardedRhs where
  prettyInternal  =
    guardedRhs

instance Pretty InjectivityInfo where
  prettyInternal x = pretty' x

instance Pretty InstDecl where
  prettyInternal i =
    case i of
      InsDecl _ d -> pretty d
      InsType _ name ty ->
        depend (do write "type "
                   pretty name
                   write " = ")
               (pretty ty)
      _ -> pretty' i

instance Pretty Match where
  prettyInternal x =
    case x of
      Match _ name pats rhs' mbinds -> do
        pretty name
        space
        spaced (map pretty pats)
        pretty rhs'
        case mbinds of
          Nothing -> return ()
          Just binds ->
            do newline
               indentedBlock $ do
                 write "where"
                 newline
                 -- TODO these binds should use fitsOnOneLine for RHS
                 indentedBlock (pretty binds)

      a -> pretty a -- infix declaration "a :+: b"

instance Pretty PatField where
  prettyInternal x =
    case x of
      PFieldPat _ n p ->
        depend (do pretty n
                   write " = ")
               (pretty p)
      PFieldPun _ n -> pretty n
      PFieldWildcard _ -> write ".."

instance Pretty QualConDecl where
  prettyInternal x =
    case x of
      QualConDecl _ tyvars ctx d ->
        depend (unless (null (fromMaybe [] tyvars))
                       (do write "forall "
                           spaced (map pretty (fromMaybe [] tyvars))
                           write ". "))
               (withCtx ctx
                       (pretty d))

instance Pretty Rhs where
  prettyInternal =
    rhs

instance Pretty Splice where
  prettyInternal x =
    case x of
      IdSplice _ str ->
        do write "$"
           string str
      ParenSplice _ e ->
        depend (write "$")
               (parens (pretty e))

instance Pretty InstRule where
  prettyInternal (IParen _ rule) = parens $ pretty rule
  prettyInternal (IRule _ mvarbinds mctx ihead) =
    do case mvarbinds of
         Nothing -> return ()
         Just xs -> do write "forall "
                       spaced (map pretty xs)
                       write ". "
       withCtx mctx (pretty ihead)

instance Pretty InstHead where
  prettyInternal x =
    case x of
      -- Base cases
      IHCon _ name -> pretty name
      IHInfix _ typ' name ->
        depend (pretty typ')
               (do space
                   prettyInfixOp name)
      -- Recursive application
      IHApp _ ihead typ' ->
        depend (pretty ihead)
               (do space
                   pretty typ')
      -- Wrapping in parens
      IHParen _ h -> parens (pretty h)

instance Pretty DeclHead where
  prettyInternal x =
    case x of
      DHead _ name -> pretty name
      DHParen _ h -> parens (pretty h)
      DHInfix _ var name ->
        do pretty var
           space
           case name of
              Ident _ _ -> do
                write "`"
                pretty name
                write "`"
              Symbol _ _ -> pretty name
      DHApp _ dhead var ->
        depend (pretty dhead)
               (do space
                   pretty var)

instance Pretty Overlap where
  prettyInternal (Overlap _) = write "{-# OVERLAP #-}"
  prettyInternal (NoOverlap _) = write "{-# NO_OVERLAP #-}"
  prettyInternal (Incoherent _) = write "{-# INCOHERENT #-}"

instance Pretty Sign where
  prettyInternal (Signless _) = return ()
  prettyInternal (Negative _) = write "-"

--------------------------------------------------------------------------------
-- * Unimplemented or incomplete printers

instance Pretty Module where
  prettyInternal x =
    case x of
      Module _ mayModHead pragmas imps decls ->
        do inter (do newline
                     newline)
                 (mapMaybe (\(isNull,r) ->
                              if isNull
                                 then Nothing
                                 else Just r)
                           [(null pragmas, inter newline (map pretty pragmas))
                           ,(case mayModHead of
                               Nothing -> (True,return ())
                               Just modHead -> (False,pretty modHead))
                           ,(null imps,formatImports imps)
                           ,(null decls
                            ,interOf newline
                                     (map (\case
                                             r@TypeSig{} -> (1,pretty r)
                                             r@InlineSig{} -> (1, pretty r)
                                             r -> (2,pretty r))
                                          decls))])
           newline
        where interOf i ((c,p):ps) =
                case ps of
                  [] -> p
                  _ ->
                    do p
                       replicateM_ c i
                       interOf i ps
              interOf _ [] = return ()
      XmlPage{} -> error "FIXME: No implementation for XmlPage."
      XmlHybrid{} -> error "FIXME: No implementation for XmlHybrid."

-- | Format imports, preserving empty newlines between them.
formatImports :: [ImportDecl NodeInfo] -> Printer ()
formatImports imps =
    mapM_ formatImport (zip [1 ..] (zip (Nothing : map Just imps) imps))
  where
    formatImport (i, (mprev, current)) = do
        when (difference > 1) newline
        pretty current
        unless (i == length imps) newline
      where
        difference =
            case mprev of
                Nothing -> 0
                Just prev ->
                    fst (srcSpanStart (srcInfoSpan (nodeInfoSpan (ann current)))) -
                    fst (srcSpanEnd   (srcInfoSpan (nodeInfoSpan (ann prev   ))))

instance Pretty Bracket where
  prettyInternal x =
    case x of
      ExpBracket _ p ->
        brackets
          (depend
             (write "|")
             (do pretty p
                 write "|"))
      PatBracket _ p ->
        brackets
          (depend
             (write "p|")
             (do pretty p
                 write "|"))
      TypeBracket _ ty ->
        brackets
          (depend
             (write "t|")
             (do pretty ty
                 write "|"))
      d@(DeclBracket _ _) -> pretty' d

instance Pretty IPBind where
  prettyInternal x =
    case x of
      IPBind _ name expr -> do
        pretty name
        space
        write "="
        space
        pretty expr

--------------------------------------------------------------------------------
-- * Fallback printers

instance Pretty DataOrNew where
  prettyInternal = pretty'

instance Pretty FunDep where
  prettyInternal = pretty'

instance Pretty Kind where
  prettyInternal = pretty'

instance Pretty ResultSig where
  prettyInternal (KindSig _ kind) = pretty kind
  prettyInternal (TyVarSig _ tyVarBind) = pretty tyVarBind

instance Pretty Literal where
  prettyInternal (String _ _ rep) = do
    write "\""
    string rep
    write "\""
  prettyInternal (Char _ _ rep) = do
    write "'"
    string rep
    write "'"
  prettyInternal (PrimString _ _ rep) = do
    write "\""
    string rep
    write "\"#"
  prettyInternal (PrimChar _ _ rep) = do
    write "'"
    string rep
    write "'#"
  -- We print the original notation (because HSE doesn't track Hex
  -- vs binary vs decimal notation).
  prettyInternal (Int _l _i originalString) =
    string originalString
  prettyInternal (Frac _l _r originalString) =
    string originalString
  prettyInternal x = pretty' x

instance Pretty Name where
  prettyInternal x = case x of
                          Ident _ _ -> pretty' x -- Identifiers.
                          Symbol _ s -> string s -- Symbols

instance Pretty QName where
  prettyInternal =
    \case
      Qual _ m n -> do
        pretty m
        write "."
        pretty n
      UnQual _ n -> pretty n
      Special _ c -> pretty c

instance Pretty SpecialCon where
  prettyInternal s =
    case s of
      UnitCon _ -> write "()"
      ListCon _ -> write "[]"
      FunCon _ -> write "->"
      TupleCon _ Boxed i ->
        string ("(" ++
                replicate (i - 1) ',' ++
                ")")
      TupleCon _ Unboxed i ->
        string ("(#" ++
                replicate (i - 1) ',' ++
                "#)")
      Cons _ -> write ":"
      UnboxedSingleCon _ -> write "(##)"

instance Pretty QOp where
  prettyInternal = pretty'

instance Pretty TyVarBind where
  prettyInternal = pretty'

instance Pretty ModuleHead where
  prettyInternal (ModuleHead _ name mwarnings mexports) =
    do write "module "
       pretty name
       maybe (return ()) pretty mwarnings
       maybe (return ())
             (\exports ->
                do
                  write " ("
                  newline
                  indented 2 (pretty exports))
             mexports
       write " where"

instance Pretty ModulePragma where
  prettyInternal = pretty'

instance Pretty ImportDecl where
  prettyInternal (ImportDecl _ m qual src safe mbPkg mbName mbSpecs) = do
    write "import"
    when src (write " {-# SOURCE #-}")
    when safe (write " safe")
    when qual (write " qualified")
    when (not qual && not src && not safe && isNothing mbPkg) (write "          ")
    maybe (pure ()) (\pk -> space *> write (show pk)) mbPkg
    space
    pretty' m
    maybe (pure ()) (\n -> write " as " *> pretty n) mbName
    maybe (pure ()) (depend space . pretty) mbSpecs


instance Pretty ModuleName where
  prettyInternal (ModuleName _ name) =
    write name

instance Pretty ImportSpecList where
  prettyInternal (ImportSpecList _ b ispecs) = do
    when b (write "hiding")
    -- TODO list should wrap in batches
    depend space . parens . commas . flip fmap ispecs $ \spec -> do
      pretty spec

instance Pretty ImportSpec where
  prettyInternal = pretty'

instance Pretty WarningText where
  prettyInternal (DeprText _ s) =
    write "{-# DEPRECATED " >> string s >> write " #-}"
  prettyInternal (WarnText _ s) =
    write "{-# WARNING " >> string s >> write " #-}"

instance Pretty ExportSpecList where
  prettyInternal (ExportSpecList _ es) = do
    depend (write " ")
           (prefixedLined "," (map pretty es))
    newline
    write ")"

instance Pretty ExportSpec where
  prettyInternal x = string " " >> pretty' x

-- Do statements need to handle infix expression indentation specially because
-- do x *
--    y
-- is two invalid statements, not one valid infix op.
stmt :: Stmt NodeInfo -> Printer ()
stmt (Qualifier _ e@(InfixApp _ a op b)) =
  do col <- fmap (psColumn . snd)
                 (sandbox (write ""))
     infixApp e a op b (Just col)
stmt (Generator _ p e) =
  do indentSpaces <- getIndentSpaces
     pretty p
     indented indentSpaces
              (dependOrNewline
                 (write " <-")
                 space
                 e
                 pretty)
stmt x = case x of
           Generator _ p e ->
             depend (do pretty p
                        write " <- ")
                    (pretty e)
           Qualifier _ e -> pretty e
           LetStmt _ binds ->
             depend (write "let ")
                    (pretty binds)
           RecStmt{} ->
             error "FIXME: No implementation for RecStmt."

-- | Make the right hand side dependent if it fits on one line,
-- otherwise send it to the next line.
dependOrNewline
  :: Printer ()
  -> Printer ()
  -> Exp NodeInfo
  -> (Exp NodeInfo -> Printer ())
  -> Printer ()
dependOrNewline left prefix right f =
  do msg <- fitsOnOneLine renderDependent
     case msg of
       Nothing -> do left
                     newline
                     (f right)
       Just st -> put st
  where renderDependent = depend left (do prefix; f right)

-- | Handle do and case specially and also space out guards more.
rhs :: Rhs NodeInfo -> Printer ()
rhs (UnGuardedRhs _ (Do _ dos)) =
  do inCase <- gets psInsideCase
     write (if inCase then " -> " else " = ")
     indentSpaces <- getIndentSpaces
     let indentation | inCase = indentSpaces
                     | otherwise = max 2 indentSpaces
     swingBy indentation
             (write "do")
             (lined (map pretty dos))
rhs (UnGuardedRhs _ e) = do
  write " "
  rhsSeparator
  newline
  indentedBlock (pretty e)
rhs (GuardedRhss _ gas) =
  do newline
     n <- getIndentSpaces
     indented n
              (lined (map (\p ->
                             do write "|"
                                pretty p)
                          gas))

-- | Implement dangling right-hand-sides.
guardedRhs :: GuardedRhs NodeInfo -> Printer ()
-- | Handle do specially.

guardedRhs (GuardedRhs _ stmts (Do _ dos)) =
  do indented 1
              (do prefixedLined
                    ","
                    (map (\p ->
                            do space
                               pretty p)
                         stmts))
     inCase <- gets psInsideCase
     write (if inCase then " -> " else " = ")
     swing (write "do")
            (lined (map pretty dos))
guardedRhs (GuardedRhs _ stmts e) = do
    mst <- fitsOnOneLine printStmts
    case mst of
      Just st -> do
        put st
        mst' <-
          fitsOnOneLine
            (do write " "
                rhsSeparator
                write " "
                pretty e)
        case mst' of
          Just st' -> put st'
          Nothing -> swingIt
      Nothing -> do
        printStmts
        swingIt
  where
    printStmts =
      indented
        1
        (do prefixedLined
              ","
              (map
                 (\p -> do
                    space
                    pretty p)
                 stmts))
    swingIt = swing (write " " >> rhsSeparator) (pretty e)

{-match :: Match NodeInfo -> Printer ()
match (Match _ name pats rhs' mbinds) =
  do depend (do pretty name
                space)
            (spaced (map pretty pats))
     withCaseContext False (pretty rhs')
     for_ mbinds bindingGroup
match (InfixMatch _ pat1 name pats rhs' mbinds) =
  do depend (do pretty pat1
                space
                case name of
                  Ident _ i ->
                    string ("`" ++ i ++ "`")
                  Symbol _ s -> string s)
            (do space
                spaced (map pretty pats))
     withCaseContext False (pretty rhs')
     for_ mbinds bindingGroup
-}

-- | Format contexts with spaces and commas between class constraints.
context :: Context NodeInfo -> Printer ()
context ctx =
  case ctx of
    CxSingle _ a -> pretty a
    CxTuple _ as -> do
      depend (write "( ") $ prefixedLined ", " (map pretty as)
      newline
      write ")"
    CxEmpty _ -> parens (return ())

unboxParens :: Printer a -> Printer a
unboxParens p =
  depend (write "(# ")
         (do v <- p
             write " #)"
             return v)

typ :: Type NodeInfo -> Printer ()
typ (TyTuple _ Boxed types) = parens $ inter (write ", ") $ map pretty types
typ (TyTuple _ Unboxed types) = unboxParens $ inter (write ", ") $ map pretty types
typ x = case x of
          TyForall _ mbinds ctx ty ->
            depend (case mbinds of
                      Nothing -> return ()
                      Just ts ->
                        do write "forall "
                           spaced (map pretty ts)
                           write ". ")
                   (do indentSpaces <- getIndentSpaces
                       withCtx ctx (indented indentSpaces (pretty ty)))
          TyFun _ a b ->
            depend (do pretty a
                       write " -> ")
                   (pretty b)
          TyTuple _ boxed tys ->
            depend (write (case boxed of
                             Unboxed -> "(#"
                             Boxed -> "("))
                   (do commas (map pretty tys)
                       write (case boxed of
                                Unboxed -> "#)"
                                Boxed -> ")"))
          TyList _ t -> brackets (pretty t)
          TyParArray _ t ->
            brackets (do write ":"
                         pretty t
                         write ":")
          a@(TyApp _ (TyApp _ _ _) _) -> linedTyApp a
          TyApp _ f a -> spaced [pretty f, pretty a]
          TyVar _ n -> pretty n
          TyCon _ p -> pretty p
          TyParen _ e -> parens (pretty e)
          TyInfix _ a op b ->
            depend (do pretty a
                       space)
                   (depend (do prettyInfixOp op
                               space)
                           (pretty b))
          TyKind _ ty k ->
            parens (do pretty ty
                       write " :: "
                       pretty k)
          TyBang _ bangty unpackty right ->
            do pretty unpackty
               pretty bangty
               pretty right
          TyEquals _ left right ->
            do pretty left
               write " ~ "
               pretty right
          ty@TyPromoted{} -> pretty' ty
          TySplice _ splice -> pretty splice
          TyWildCard _ name ->
            case name of
              Nothing -> write "_"
              Just n ->
                do write "_"
                   pretty n
          TyQuasiQuote _ n s ->
            brackets (depend (do string n
                                 write "|")
                             (do string s
                                 write "|"))

linedTyApp :: Type NodeInfo -> Printer ()
linedTyApp a =
  case typeList a of
    [] -> pretty a
    ls@(x:xs) ->
      ifFitsOnOneLineOrElse
        (spaced (fmap pretty ls))
        (swing (pretty x) ((spacedWrap (fmap pretty xs))))

typeList :: Type NodeInfo -> [Type NodeInfo]
typeList x = case x of
  TyApp _ a@(TyCon _ _) b -> a : typeList b
  TyApp _ a@(TyApp _ _ _) b -> typeList a <> typeList b
  y -> [y]

-- | Specially format records. Indent where clauses only 2 spaces.
decl' :: Decl NodeInfo -> Printer ()
-- | Pretty print type signatures like
--
-- foo :: (Show x, Read x)
--     => (Foo -> Bar)
--     -> Maybe Int
--     -> (Char -> X -> Y)
--     -> IO ()
--
decl' (TypeSig _ names ty') =
  do mst <- fitsOnOneLine (declTy ty')
     case mst of
       Just{} -> depend (do inter (write ", ")
                                  (map pretty names)
                            write " :: ")
                          (declTy ty')
       Nothing -> do inter (write ", ")
                           (map pretty names)
                     write " ::"
                     newline
                     indentSpaces <- getIndentSpaces
                     indented indentSpaces (depend (write "   ") (declTy ty'))

  where declTy dty =
          case dty of
            TyForall _ mbinds mctx ty ->
              do case mbinds of
                   Nothing -> return ()
                   Just ts ->
                     do write "forall "
                        spaced (map pretty ts)
                        write "."
                        newline
                 case mctx of
                   Nothing -> prettyTy ty
                   Just ctx ->
                     do pretty ctx
                        newline
                        indented (-3)
                                 (depend (write "=> ")
                                         (prettyTy ty))
            _ -> prettyTy dty
        collapseFaps (TyFun _ arg result) = arg : collapseFaps result
        collapseFaps e = [e]
        prettyTy ty =
          do mst <- fitsOnOneLine (pretty ty)
             case mst of
               Nothing -> case collapseFaps ty of
                            [] -> pretty ty
                            tys ->
                              prefixedLined "-> "
                                            (map pretty tys)
               Just st -> put st
decl' (PatBind _ pat rhs' mbinds) =
  withCaseContext False $
    do pretty pat
       pretty rhs'
       for_ mbinds bindingGroup

-- | Handle records specially for a prettier display (see guide).
decl' (DataDecl _ dataornew ctx dhead condecls@[_] mderivs)
  | any isRecord condecls =
    do
      depend
        (do
           pretty dataornew
           unless (null condecls) space)
        (withCtx ctx (do
          pretty dhead
          multiCons condecls))
      case mderivs of
        Nothing ->
          return ()
        Just derivs ->
          space >> pretty derivs
  where
    -- this is only data types with records. :?
    multiCons xs =
      depend
        (write " =")
        (inter (write "|") (map (depend space . qualConDecl) xs))
decl' e = declx e

-- | Use special record display, used by 'dataDecl' in a record scenario.
qualConDecl :: QualConDecl NodeInfo -> Printer ()
qualConDecl x =
  case x of
    QualConDecl _ tyvars ctx d ->
      depend (unless (null (fromMaybe [] tyvars))
                     (do write "forall "
                         spaced (map pretty (fromMaybe [] tyvars))
                         write ". "))
             (withCtx ctx (recDecl d))

-- | Fields are preceded with a space.
conDecl :: ConDecl NodeInfo -> Printer ()
conDecl (RecDecl _ name fields) =
  depend (do pretty name
             write " ")
         (do depend (write "{")
                    (prefixedLined ", "
                                   (map (depend space . pretty) fields))
             write "}")
conDecl x = case x of
              ConDecl _ name bangty ->
                swing (pretty name) (spacedWrap (map pretty bangty))
              InfixConDecl l a f b ->
                pretty (ConDecl l f [a,b])
              RecDecl _ name fields ->
                depend (do pretty name
                           space)
                       (do depend (write "{")
                                  (prefixedLined ", "
                                                 (map pretty fields))
                           write "}")

-- | Record decls are formatted like: Foo
-- { bar :: X
-- }
recDecl :: ConDecl NodeInfo -> Printer ()
recDecl (RecDecl _ name fields) =
  do
     indentSpaces <- getIndentSpaces
     newline
     column indentSpaces $ depend (pretty name) (write " {")
     newline
     column (indentSpaces + indentSpaces)
            (do depend (write " ")
                       (prefixedLined ","
                                      (map (depend space . pretty) fields))
                newline
                write "}")
recDecl r = prettyInternal r

recUpdateExpr :: Printer () -> [FieldUpdate NodeInfo] -> Printer ()
recUpdateExpr expWriter updates = do
  ifFitsOnOneLineOrElse hor $ do
    expWriter
    newline
    updatesHor `ifFitsOnOneLineOrElse` updatesVer
  where
    hor = do
      expWriter
      space
      updatesHor
    updatesHor = braces $ commas $ map pretty updates
    updatesVer = do
      depend (write "{ ") $ prefixedLined ", " $ map pretty updates
      newline
      write "}"

--------------------------------------------------------------------------------
-- Predicates

-- | Is the decl a record?
isRecord :: QualConDecl t -> Bool
isRecord (QualConDecl _ _ _ RecDecl{}) = True
isRecord _ = False

-- | Does printing the given thing overflow column limit? (e.g. 80)
fitsOnOneLine :: Printer a -> Printer (Maybe PrintState)
fitsOnOneLine p =
  do st <- get
     put st { psHardLimit = True}
     ok <- fmap (const True) p <|> return False
     st' <- get
     put st
     return (if ok
                then Just st' { psHardLimit = psHardLimit st }
                else Nothing)

-- | If first printer fits, use it, else use the second one.
ifFitsOnOneLineOrElse :: Printer a -> Printer a -> Printer a
ifFitsOnOneLineOrElse a b = do
  stOrig <- get
  put stOrig{psHardLimit = True}
  res <- fmap Just a <|> return Nothing
  case res of
    Just r -> do
      modify $ \st -> st{psHardLimit = psHardLimit stOrig}
      return r
    Nothing -> do
      put stOrig
      b

bindingGroup :: Binds NodeInfo -> Printer ()
bindingGroup binds =
  do newline
     indented 2
              (do write "where"
                  newline
                  indented 2 (pretty binds))

infixApp :: Exp NodeInfo
         -> Exp NodeInfo
         -> QOp NodeInfo
         -> Exp NodeInfo
         -> Maybe Int64
         -> Printer ()
infixApp e a op b indent =
  hor `ifFitsOnOneLineOrElse` ver
  where
    hor =
      spaced
        [ case link of
          OpChainExp e' -> pretty e'
          OpChainLink qop -> pretty qop
        | link <- flattenOpChain e
        ]
    ver = do
      prettyWithIndent a
      space
      pretty op
      case b of
        Lambda{} -> space >> pretty b
        LCase{} -> space >> pretty b
        Do _ stmts -> swing (write " do") $ lined (map pretty stmts)
        _ -> do
          newline
          case indent of
            Nothing -> prettyWithIndent b
            Just col -> do
              indentSpaces <- getIndentSpaces
              column (col + indentSpaces) (prettyWithIndent b)
    prettyWithIndent e' =
      case e' of
        InfixApp _ a' op' b' -> infixApp e' a' op' b' indent
        _ -> pretty e'

-- | A link in a chain of operator applications.
data OpChainLink l
  = OpChainExp (Exp l)
  | OpChainLink (QOp l)
  deriving (Show)

-- | Flatten a tree of InfixApp expressions into a chain of operator
-- links.
flattenOpChain :: Exp l -> [OpChainLink l]
flattenOpChain (InfixApp _ left op right) =
  flattenOpChain left <>
  [OpChainLink op] <>
  flattenOpChain right
flattenOpChain e = [OpChainExp e]
