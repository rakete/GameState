{-# OPTIONS_GHC -fglasgow-exts -fth #-}
module Parser.ColladaParser where

import Text.XML.HXT.Arrow

import Utility.Tuple

data Collada = Collada
    { collada_version :: Maybe String
    , collada_xmlns :: Maybe String
    , collada_asset :: Asset
    , collada_animations :: Maybe (Library Animation)
    --, collada_animationclips :: Maybe (Library AnimationClip)
    --, collada_cameras :: Maybe (Library Camera)
    --, collada_controllers :: Maybe (Library Controller)
    --, collada_effects :: Maybe (Library Effect)
    --, collada_forecefields :: Maybe (Library ForceField)
    --, collada_geometries :: Maybe (Library Geometry)
    --, collada_images :: Maybe (Library Image)
    --, collada_lights :: Maybe (Library Light)
    --, collada_materials :: Maybe (Library Material)
    --, collada_nodes :: Maybe (Library Node)
    --, collada_scene :: Maybe Scene
    --, collada_extra :: Maybe [Extra]
    }
    deriving (Show,Eq)

instance XmlPickler Collada where
    xpickle = xpCollada

xpCollada =
    xpWrap ( \(a,b,c,d) -> Collada a b c d, \(Collada a b c d) -> (a,b,c,d) ) $
    xpElem "COLLADA" $
    xp4Tuple (xpOption $ xpAttr "version" xpText0)
            (xpOption $ xpAttr "xmlns" xpText0)
            xpAsset
            (xpOption xpickle)

--
--

data Asset = Asset
    { asset_contributor :: Maybe [Contributor]
    , asset_created :: String
    , asset_keywords :: Maybe String
    , asset_modified :: String
    , asset_revision :: Maybe String
    , asset_subject :: Maybe String
    , asset_title :: Maybe String
    , asset_unit :: (Float,String)
    , asset_up_axis :: String
    }
    deriving (Eq,Show)

instance XmlPickler Asset where
    xpickle = xpAsset

xpAsset =
    xpWrap ( \(a,b,c,d,e,f,g,h,i) -> Asset a b c d e f g h i, \(Asset a b c d e f g h i) -> (a,b,c,d,e,f,g,h,i) ) $
    xpElem "asset" $
    xp9Tuple (xpOption $ xpList xpContributor)
            (xpElem "created" xpText)
            (xpOption $ xpElem "keywords" xpText)
            (xpElem "modified" xpText)
            (xpOption $ xpElem "revision" xpText)
            (xpOption $ xpElem "subject" xpText)
            (xpOption $ xpElem "title" xpText)
            (xpElem "unit" $ xpPair
                (xpDefault 1.0 $ xpAttr "meter" xpPrim)
                (xpDefault "meter" $ xpAttr "name" xpText) )
            (xpDefault "Y_UP" $ xpElem "up_axis" xpText)
--
--

data Contributor = Contributor
    { contributor_author :: String
    , contributor_authoring_tool :: String
    , contributor_comments :: String
    , contributor_copyright :: String
    , contributor_source_data :: String }
    deriving (Eq,Show)

instance XmlPickler Contributor where
    xpickle = xpContributor

xpContributor =
    xpWrap ( \(a,b,c,d,e) -> Contributor a b c d e , \(Contributor a b c d e) -> (a,b,c,d,e) ) $
    xpElem "contributor" $
    xp5Tuple (xpElem "author" xpText0)
            (xpElem "authoring_tool" xpText0)
            (xpElem "comments" xpText0)
            (xpElem "copyright" xpText0)
            (xpElem "source_data" xpText0)

--
--

data Extra = Extra
    { extra_id :: Maybe String
    , extra_name :: Maybe String
    , extra_type :: Maybe String
    , extra_asset :: Maybe Asset
    , extra_techniques :: [Technique] }
    deriving (Eq,Show)

instance XmlPickler Extra where
    xpickle = xpExtra

xpExtra =
    xpWrap ( $(fromTuple ''Extra), $(toTuple ''Extra) ) $
    xpElem "extra" $
    xp5Tuple (xpOption $ xpAttr "id" xpText)
            (xpOption $ xpAttr "name" xpText)
            (xpOption $ xpAttr "type" xpText)
            (xpOption xpAsset)
            (xpList xpTechnique)

--
--

data Technique = Technique
    { technique_profile :: String
    , technique_xmlns :: Maybe String
    , technique_xml :: Maybe String }
    deriving (Eq,Show)

instance XmlPickler Technique where
    xpickle = xpTechnique

xpTechnique =
    xpWrap ( $(fromTuple ''Technique), $(toTuple ''Technique) ) $
    xpElem "technique" $
    xpTriple (xpAttr "profile" xpText)
            (xpOption $ xpAttr "xmlns" xpText)
            (xpOption $ xpXmlText)

--
--

data SourceTechniqueCommon =
    SourceTechniqueCommon
        { sourcetechniquecommon_accessor :: Accessor }
    deriving (Eq,Show)

instance XmlPickler SourceTechniqueCommon where
    xpickle = xpSourceTechniqueCommon

xpSourceTechniqueCommon =
    xpWrap ( SourceTechniqueCommon , sourcetechniquecommon_accessor ) $
    xpElem "technique_common" $
    xpAccessor

--
--

data Param = Param
    { param_name :: Maybe String
    , param_sid :: Maybe String
    , param_type :: String
    , param_semantic :: Maybe String }
    deriving (Eq,Show)

instance XmlPickler Param where
    xpickle = xpParam

xpParam =
    xpWrap ( $(fromTuple ''Param), $(toTuple ''Param) ) $
    xpElem "param" $
    xp4Tuple (xpOption $ xpAttr "name" xpText)
            (xpOption $ xpAttr "sid" xpText)
            (xpAttr "type" xpText)
            (xpOption $ xpAttr "semantic" xpText)

--
--

data Library a = Library
    { library_id :: Maybe String
    , library_name :: Maybe String
    , library_asset :: Maybe Asset
    , library_contents :: [a]
    , library_extra :: Maybe [Extra]
    }
    deriving (Eq,Show)

xpLibrary pucontents =
    xpWrap ( $(fromTuple ''Library), $(toTuple ''Library) ) $
    xpElem "library_animations" $
    xp5Tuple (xpOption $ xpAttr "id" xpText)
            (xpOption $ xpAttr "name" xpText)
            (xpOption xpAsset)
            pucontents
            (xpOption $ xpList xpExtra)

instance XmlPickler (Library Animation) where
    xpickle = xpLibrary (xpList xpAnimation)

--
--

data Animation = Animation
    { animation_id :: Maybe String
    , animation_name :: Maybe String
    , animation_asset :: Maybe Asset
    , animation_animations :: Maybe [Animation]
    , animation_sources :: Maybe [Source]
    , animation_sampler :: Maybe [Sampler]
    , animation_channel :: Maybe [(String,String)]
    , animation_extra :: Maybe [Extra]
    }
    deriving (Eq,Show)

instance XmlPickler Animation where
    xpickle = xpAnimation

xpAnimation =
    xpWrap ( $(fromTuple ''Animation), $(toTuple ''Animation) ) $
    xpElem "animation" $
    xp8Tuple (xpOption $ xpAttr "id" xpText)
            (xpOption $ xpAttr "name" xpText)
            (xpOption xpAsset)
            (xpOption $ xpList xpAnimation)
            (xpOption $ xpList xpSource)
            (xpOption $ xpList xpSampler)
            (xpOption $ xpList $ xpElem "channel" $ xpPair (xpAttr "source" xpText) (xpAttr "target" xpText))
            (xpOption $ xpList xpExtra)

--
--

data Source = Source
    { source_id :: String
    , source_name :: Maybe String
    , source_asset :: Maybe Asset
    , source_array :: Maybe ArrayType
    , source_techniquecommon :: Maybe SourceTechniqueCommon
    , source_techniques :: Maybe [Technique] }
    deriving (Eq,Show)

instance XmlPickler Source where
    xpickle = xpSource

xpSource =
    xpWrap ( $(fromTuple ''Source), $(toTuple ''Source) ) $
    xpElem "source" $
    xp6Tuple (xpAttr "id" xpText)
            (xpOption $ xpAttr "name" xpText)
            (xpOption xpAsset)
            (xpOption xpArrayType)
            (xpOption xpSourceTechniqueCommon)
            (xpOption $ xpList xpTechnique)

--
--

data ArrayType =
    FloatArray
        { floatarray_count :: Int
        , floatarray_id :: Maybe String
        , floatarray_name :: Maybe String
        , floatarray_digits :: Maybe Int
        , floatarray_magnitude :: Maybe Int
        , floatarray :: [Float] } |
    IntArray 
        { intarray_count :: Int
        , intarray_id :: Maybe String
        , intarray_name :: Maybe String
        , intarray_min :: Maybe Int
        , intarray_max :: Maybe Int
        , intarray :: [Int] } |
    NameArray
        { namearray_count :: Int
        , namearray_id :: Maybe String
        , namearray_name :: Maybe String
        , namearray :: [String] } |
    IDRefArray
        { idrefarray_count :: Int
        , idrefarray_id :: Maybe String
        , idrefarray_name :: Maybe String
        , idrefarray :: [String] }
    deriving (Eq,Show)

instance XmlPickler ArrayType where
    xpickle = xpArrayType

xpArrayType =
    xpChoiceList
        [ xpWrap ( \(a,b,c,d,e,f) -> FloatArray a b c d e f, \(FloatArray a b c d e f) -> (a,b,c,d,e,f) ) $
          xpElem "float_array" $ xp6Tuple
            (xpAttr "count" xpPrim)
            (xpOption $ xpAttr "id" xpText)
            (xpOption $ xpAttr "name" xpText)
            (xpOption $ xpAttr "digits" xpPrim)
            (xpOption $ xpAttr "magnitude" xpPrim)
            xpPrimList
        , xpWrap ( \(a,b,c,d,e,f) -> IntArray a b c d e f, \(IntArray a b c d e f) -> (a,b,c,d,e,f) ) $
          xpElem "int_array" $ xp6Tuple
            (xpAttr "count" xpPrim)
            (xpOption $ xpAttr "id" xpText)
            (xpOption $ xpAttr "name" xpText)
            (xpOption $ xpAttr "minInclusive" xpPrim)
            (xpOption $ xpAttr "maxInclusive" xpPrim)
            xpPrimList
        , xpWrap ( \(a,b,c,d) -> NameArray a b c d, \(NameArray a b c d) -> (a,b,c,d) ) $
        xpElem "Name_array" $ xp4Tuple
            (xpAttr "count" xpPrim)
            (xpOption $ xpAttr "id" xpText)
            (xpOption $ xpAttr "name" xpText)
            xpTextList
        , xpWrap ( \(a,b,c,d) -> IDRefArray a b c d, \(IDRefArray a b c d) -> (a,b,c,d) ) $
          xpElem "IDRef_array" $ xp4Tuple
            (xpAttr "count" xpPrim)
            (xpOption $ xpAttr "id" xpText)
            (xpOption $ xpAttr "name" xpText)
            xpTextList ]

{-
xpArrayType =
    xpChoice
        ( xpWrap ( \(a,b,c,d,e,f) -> FloatArray a b c d e f, \(FloatArray a b c d e f) -> (a,b,c,d,e,f) ) $
          xpElem "float_array" $ xp6Tuple
            (xpAttr "count" xpPrim)
            (xpOption $ xpAttr "id" xpText)
            (xpOption $ xpAttr "name" xpText)
            (xpOption $ xpAttr "digits" xpPrim)
            (xpOption $ xpAttr "magnitude" xpPrim)
            xpPrimList )
        ( xpChoice
            ( xpWrap ( \(a,b,c,d,e,f) -> IntArray a b c d e f, \(IntArray a b c d e f) -> (a,b,c,d,e,f) ) $
              xpElem "int_array" $ xp6Tuple
                (xpAttr "count" xpPrim)
                (xpOption $ xpAttr "id" xpText)
                (xpOption $ xpAttr "name" xpText)
                (xpOption $ xpAttr "minInclusive" xpPrim)
                (xpOption $ xpAttr "maxInclusive" xpPrim)
                xpPrimList )
            ( xpChoice
                ( xpWrap ( \(a,b,c,d) -> NameArray a b c d, \(NameArray a b c d) -> (a,b,c,d) ) $
                xpElem "Name_array" $ xp4Tuple
                    (xpAttr "count" xpPrim)
                    (xpOption $ xpAttr "id" xpText)
                    (xpOption $ xpAttr "name" xpText)
                    xpTextList )
                ( xpWrap ( \(a,b,c,d) -> IDRefArray a b c d, \(IDRefArray a b c d) -> (a,b,c,d) ) $
                  xpElem "IDRef_array" $ xp4Tuple
                    (xpAttr "count" xpPrim)
                    (xpOption $ xpAttr "id" xpText)
                    (xpOption $ xpAttr "name" xpText)
                    xpTextList )
                xpLift )
            xpLift )
        xpLift
-}

--
--

data Accessor = Accessor
    { accessor_count :: Int
    , accessor_offset :: Maybe Int
    , accessor_source :: String
    , accessor_stride :: Maybe Int
    , accessor_params :: Maybe [Param] }
    deriving (Eq,Show)

instance XmlPickler Accessor where
    xpickle = xpAccessor

xpAccessor =
    xpWrap ( $(fromTuple ''Accessor), $(toTuple ''Accessor) ) $
    xpElem "accessor" $
    xp5Tuple (xpAttr "count" xpPrim)
            (xpOption $ xpAttr "offset" xpPrim)
            (xpAttr "source" xpText)
            (xpOption $ xpAttr "stride" xpPrim)
            (xpOption $ xpList xpParam)

{-InfixE
    (Just (AppE (VarE Text.XML.HXT.Arrow.Pickle.Xml.xpWrap)
        (TupE
            [LamE [TupP [VarP a_0,VarP b_1,VarP c_2,VarP d_3]]
                (AppE (AppE (AppE (AppE (ConE Parser.ColladaParser.Accessor)
                    (VarE a_0)) (VarE b_1)) (VarE c_2)) (VarE d_3)),
                LamE [ConP Parser.ColladaParser.Accessor [VarP a_4,VarP b_5,VarP c_6,VarP d_7]]
                    (TupE [VarE a_4,VarE b_5,VarE c_6,VarE d_7])])))
    (VarE GHC.Base.$) (Just (InfixE (Just (AppE (VarE Text.XML.HXT.Arrow.Pickle.Xml.xpElem) (LitE (StringL "accessor")))) 
    (VarE GHC.Base.$) (Just (AppE (AppE (AppE (AppE (VarE Text.XML.HXT.Arrow.Pickle.Xml.xp4Tuple)
        (AppE (AppE (VarE Text.XML.HXT.Arrow.Pickle.Xml.xpAttr) (LitE (StringL "count"))) (VarE Text.XML.HXT.Arrow.Pickle.Xml.xpPrim)))
        (AppE (AppE (VarE Text.XML.HXT.Arrow.Pickle.Xml.xpAttr) (LitE (StringL "source"))) (VarE Text.XML.HXT.Arrow.Pickle.Xml.xpText)))
        (AppE (AppE (VarE Text.XML.HXT.Arrow.Pickle.Xml.xpAttr) (LitE (StringL "stride"))) (VarE Text.XML.HXT.Arrow.Pickle.Xml.xpPrim))) 
        (AppE (VarE Text.XML.HXT.Arrow.Pickle.Xml.xpList)
            (AppE (AppE (VarE Text.XML.HXT.Arrow.Pickle.Xml.xpElem) (LitE (StringL "param")))
                (AppE (AppE (VarE Text.XML.HXT.Arrow.Pickle.Xml.xpPair)
                    (AppE (AppE (VarE Text.XML.HXT.Arrow.Pickle.Xml.xpAttr) (LitE (StringL "type"))) (VarE Text.XML.HXT.Arrow.Pickle.Xml.xpText)))
                    (AppE (AppE (VarE Text.XML.HXT.Arrow.Pickle.Xml.xpAttr) (LitE (StringL "name"))) (VarE Text.XML.HXT.Arrow.Pickle.Xml.xpText)))))))))-}

--
--

data Sampler = Sampler
    { sampler_id :: Maybe String
    , sampler_inputs :: [(String,String)] }
    deriving (Eq,Show)

instance XmlPickler Sampler where
    xpickle = xpSampler

xpSampler =
    xpWrap (\(a,b) -> Sampler a b, \(Sampler a b) -> (a,b) ) $
    xpElem "sampler" $
    xpPair (xpOption $ xpAttr "id" xpText)
           (xpList1 (xpElem "input" (xpPair (xpAttr "semantic" xpText) (xpAttr "source" xpText))))

--
--

testfoo :: IO ()
testfoo = do
    runX (
        xunpickleDocument xpCollada [ (a_validate,v_0), (a_trace, v_1), (a_remove_whitespace,v_1), (a_preserve_comment, v_0)] "bonefoo6.dae"
        >>> processFoo
        {- >>> xpickleDocument xpCollada [ (a_indent, v_1)] "blah.dae"-} )
    return ()

processFoo :: IOSArrow Collada Collada
processFoo = arrIO ( \ x -> do {print x ; return x})

--
--

xpPrimList :: (Show a, Read a) => PU [a]
xpPrimList = xpWrap (\s -> map read $ words s, \xs -> unwords $ map show xs ) xpText

xpTextList :: PU [String]
xpTextList = xpWrap (\s -> words s, \xs -> unwords xs ) xpText

xpChoiceList :: [PU a] -> PU a
xpChoiceList pus = foldl1 (\a' a -> xpChoice a a' xpLift) pus

xp7Tuple :: PU a -> PU b -> PU c -> PU d -> PU e -> PU f -> PU g -> PU (a, b, c, d, e, f, g)
xp7Tuple pa pb pc pd pe pf pg
    = xpWrap (toSeven, fromSeven) (xpPair pa (xpPair pb (xpPair pc (xpPair pd (xpPair pe (xpPair pf pg))))))
    where
    toSeven   ~(a, ~(b, ~(c, ~(d, ~(e, ~(f, g)))))) = (a,  b,  c,  d,  e, f, g)
    fromSeven ~(a,   b,   c,   d,   e,   f, g)      = (a, (b, (c, (d, (e, (f, g))))))

xp8Tuple :: PU a -> PU b -> PU c -> PU d -> PU e -> PU f -> PU g -> PU h -> PU (a, b, c, d, e, f, g, h)
xp8Tuple pa pb pc pd pe pf pg ph
    = xpWrap (toEight, fromEight) (xpPair pa (xpPair pb (xpPair pc (xpPair pd (xpPair pe (xpPair pf (xpPair pg ph)))))))
    where
    toEight   ~(a, ~(b, ~(c, ~(d, ~(e, ~(f, ~(g, h))))))) = (a,  b,  c,  d,  e, f, g, h)
    fromEight ~(a,   b,   c,   d,   e,   f,   g,   h)     = (a, (b, (c, (d, (e, (f, (g, h)))))))

xp9Tuple :: PU a -> PU b -> PU c -> PU d -> PU e -> PU f -> PU g -> PU h -> PU i -> PU (a, b, c, d, e, f, g, h, i)
xp9Tuple pa pb pc pd pe pf pg ph pi
    = xpWrap (toNine, fromNine) (xpPair pa (xpPair pb (xpPair pc (xpPair pd (xpPair pe (xpPair pf (xpPair pg (xpPair ph pi))))))))
    where
    toNine   ~(a, ~(b, ~(c, ~(d, ~(e, ~(f, ~(g, ~(h, i)))))))) = (a,  b,  c,  d,  e, f, g, h, i)
    fromNine ~(a,   b,   c,   d,   e,   f,   g,   h, i)      = (a, (b, (c, (d, (e, (f, (g, (h, i))))))))
