// loadDependencies :: Cache -> [String] -> Dependencies -> FallibleIO Dependencies
// loadDependencies _ [] deps = return deps
// loadDependencies cache (path : upper) deps = do
//   (cache, LoadedFile file zybas phps) <- lookupCache ZybaLang cache path
//   (cache, phpContents) <- getPhps cache phps
//   let children = map (deps Map.!) zybas ++ zipWith (curry leaf) phps phpContents
//   case filter (`Map.notMember` deps) zybas of
//     [] -> loadDependencies cache upper $ Map.insert path (Tree.Node (path, file) children) deps
//     next : _ -> checkCircular next >> loadDependencies cache (next : path : upper) deps
//   where getPhps = mapCatFoldlM (\cache' path' -> fmap2 id (listify . getFile) $ lookupCache PhpLang cache' path')
//         getFile (LoadedFile file _ _) = file
//         checkCircular path = if path `elem` upper then wrap $ err (-1) $ "Circular dependency: " ++ path else return ()

// orderDependencies :: Ord a => Tree.Tree (a, b) -> [(a, b)]
// orderDependencies = nubOrdOn fst . concat . reverse . Tree.levels

// listOfImportedFiles :: String -> FallibleIO [(String, File)]
// listOfImportedFiles path = do
//   deps <- loadDependencies Map.empty [path] Map.empty 
//   return $ orderDependencies $ deps Map.! path

// analyseAll :: Map.Map String Scope.Scope -> [String] -> [(String, [Semantics.Declaration])] -> [(String, File)] -> Fallible ([String], [(String, [Semantics.Declaration])])
// analyseAll _ phps zybas [] = return $ (reverse phps, reverse zybas)
// analyseAll known phps zybas ((_, Php content) : paths) = analyseAll known (content : phps) zybas paths
// analyseAll known phps zybas ((path, Zyba parsed) : paths) = do
//   (scope, declarations) <- Semantics.analyse known path parsed
//   analyseAll (Map.insert path scope known) phps ((path, declarations) : zybas) paths

// load :: String -> FallibleIO ([String], [(String, [Semantics.Declaration])])
// load path = listOfImportedFiles path >>= wrap . analyseAll Map.empty [] []

use std::collections::HashMap;
use std::fs;
use std::path::Path;

use crate::error::{Fallible, err};
use crate::parser;
use crate::semantics;

struct Module {
    decls: Vec<parser::Decl>,
    imports: Vec<String>
}

type Cache = HashMap<String, Module>;

fn propagate_err<T, E: std::fmt::Display>(r: Result<T, E>) -> Fallible<T> {
    match r {
        Ok(value) => Ok(value),
        Err(error) => err(0, error.to_string())
    }
}

fn parse_file(path: &str) -> Fallible<Vec<parser::Decl>> {
    match fs::read_to_string(path) {
        Ok(input) => parser::parse(&input),
        Err(error) => err(0, error.to_string())?
    }
}

fn to_absolute(importer: &str, importee: &str) -> String {
    let importer = Path::new(importer);
    if Path::new(importee).is_absolute() {
        importee.to_string()
    } else {
        let dir = importer.parent().unwrap_or(importer);
        let absolute = dir.join(importee);
        absolute.to_string_lossy().to_string()
    }
}

fn load_module(path: &str) -> Fallible<Module> {
    let mut decls = parse_file(path)?;
    let mut imports = vec![];
    for i in 0..decls.len() {
        if let parser::Decl::Import { line, path: import } = &decls[i] {
            let abs_path = to_absolute(path, import.as_str());
            imports.push(abs_path.clone());
            decls[i] = parser::Decl::Import { line: *line, path: abs_path };
        }
    }
    Ok(Module { decls, imports })
}

fn lookup_cache<'a>(cache: &'a mut Cache, path: &str) -> Fallible<&'a Module> {
    if !cache.contains_key(path) {
        let file = load_module(path)?;
        cache.insert(path.to_string(), file);
    }
    Ok(&cache[path])
}
