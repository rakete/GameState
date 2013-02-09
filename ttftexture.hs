import System
import Char
import List
import Graphics.Imlib

clearImage (ImlibColor a r g b) = (ImlibColor 0 0 0 0)

createGlyphTexture :: String -> String -> FilePath -> IO ()
createGlyphTexture fontstring text texturepath = do
    let fontname = takeWhile (not . (=='/')) fontstring

    font <- loadFont fontstring
    contextSetFont font

    dims <- sequence $ map (\c -> do { (x,y) <- getTextSize [c]; i <- getTextInset [c]; return (x,y,i)}) text
    print dims
    let (xs,ys,is) = unzip3 dims
    advance <- getTextAdvance [head text]

    let (glyph_w,glyph_h) = (maximum xs, maximum ys)

    let numrows = foldl1 (\nr' nr ->
                    let x1 = nr*glyph_w
                        y1 = let (l,i) = (length text `divMod` nr)
                             in (l + (if i > 0 then 1 else 0))*glyph_h
                        a = abs(x1 - y1)
                        x2 = nr'*glyph_w
                        y2 = let (l,i) = (length text `divMod` nr')
                             in (l + (if i > 0 then 1 else 0))*glyph_h
                        b = abs(x2 - y2)
                    in if a < b
                        then nr
                        else nr') $ [1..(length text)]

    let numlines = let (l,i) = (length text `divMod` numrows)
                   in l + (if i > 0 then 1 else 0)

    let (texture_w,texture_h) = (numrows * glyph_w, numlines * glyph_h)

    let (space_w,space_h) = case (texture_w < texture_h, texture_h < texture_w) of
                                (True,False) -> 
                                    let sw = (texture_h-texture_w) `div` numrows
                                        sh = if numrows * (glyph_w+sw) == texture_h
                                              then 0
                                              else if (numrows * (glyph_w+sw)) > texture_h
                                                    then (numrows*(glyph_w+sw)-texture_h) `div` numlines
                                                    else 0
                                    in (sw,sh)
                                (False,True) ->
                                    let sh = (texture_w-texture_h) `div` numlines
                                        sw = if numlines * (glyph_h+sh) == texture_w
                                              then 0
                                              else if (numlines * (glyph_h+sh)) > texture_w
                                                    then (numlines*(glyph_h+sh)-texture_w) `div` numrows
                                                    else 0
                                    in (sw,sh)
                                otherwise -> (0,0)

    let (texture_w',texture_h') = (numrows * (glyph_w+space_w), numlines * (glyph_h+space_h))

    putStrLn $ "GlyphSize : " ++ show (glyph_w,glyph_h)
    putStrLn $ "Spacing : " ++ show (space_w,space_h)
    putStrLn $ "Numrows/lines : " ++ show (numrows,numlines)
    putStrLn $ "Texturesize : " ++ show (texture_w',texture_h')

    image <- createImage texture_w' texture_h'
    contextSetImage image
    imageSetHasAlpha True
    imageSetFormat "png"
    imageSetBorder $ ImlibBorder 0 0 0 0
    sequence $ [textDraw x y [c] | ((x,y),c) <- zip [(x,y) | y <- [0,(glyph_h+space_h)..texture_h'], x <- [0,(glyph_w+space_w)..(texture_w'-(glyph_w+space_w))]] text]
    saveImage texturepath
    return ()

split :: Eq a=> a -> [a] -> [[a]]
split c s = foldl (\a b -> if b==c then (a ++ [[]]) else (init a) ++ [(last a) ++ [b]]) [[]] s

main = do
    args <- getArgs
    temp <- case length args of
        3 -> do
            let (fp:fs:o:[]) = args
            let t = (map chr [32..126])
            return (False,fp,fs,t,o)
        4 -> do
            let (fp:fs:t:o:[]) = args
            return (False,fp,fs,t,o)
        otherwise -> return (True,"","","","")
    let (exit,fontpaths,fontstring,text,outputfile) = temp

    if exit
     then do
        putStrLn "usage: ttftexture /path/to/your/font fontname/fontsize optional_text output_image.png"
        return ()
     else do
        --let fontpaths = "/usr/share/fonts/truetype/freefont"
        --let fontstring = "FreeSans/30"
        --let outputfile = "foo.png"
        --let text = map chr [32..126]

        sequence $ map addPathToFontPath $ split ':' fontpaths
        fontlist <- listFonts
        pathlist <- listFontPath
        putStrLn $ show pathlist
        putStrLn $ show fontlist

        let fontname = takeWhile (not . (=='/')) fontstring

        if elem fontname fontlist
         then do
            createGlyphTexture fontstring text outputfile
         else do
            return ()

