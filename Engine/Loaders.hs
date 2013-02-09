
module Engine.Loaders

import Engine.Factory
import Engine.Geometry
import Parser.ObjParser

data ObjMeshLoader = ObjMeshLoader (ObjScene GLfloat GLuint) String

instance Factory ObjMeshLoader (Maybe Mesh) where
    construct (objscene,name) = do
        let objectsmap = objects objscene
        mtllib <- case mtllib objscene of
                    Just ml -> construct ml
                    Nothing -> return M.empty
        m <- IM.foldWithKey (uploadByName mtllib) (return M.empty) objectsmap
        return $ M.lookup name $
            catMaybesMap $ M.map (\e ->
                case e of
                    Left o -> Just o
                    Right ps -> Nothing
                ) m

        where

        uploadByName mtllib k objmesh io = do
            m <- io
            let (name:pose:[]) = take 2 $ (split '_' $ objmesh_name objmesh) ++ (repeat [])
            case (name,pose) of
                (xs,[]) -> do
                    putStrLn $ "Uploading Object " ++ (objmesh_name objmesh)
                    obj <- vboUploadObject mtllib objmesh
                    let m' = M.insertWith (\(Left o') e ->
                                case e of
                                    (Right ps) -> Left $ (\o -> o{ mesh_poses = ps ++ (mesh_poses o)}) o'
                                    (Left _) -> e -- we could handle duplicate names here, but we don't do that yet
                             ) xs (Left obj) m
                    return m'
                (xs,ys) -> do
                    putStrLn $ "Uploading Pose " ++ (objmesh_name objmesh)
                    pose <- vboUploadObjectPose objmesh
                    let m' = M.insertWith (\(Right p) e ->
                                case e of
                                    (Right ps) -> Right $ ps ++ p
                                    (Left o') -> Left $ (\o -> o{ mesh_poses = (mesh_poses o) ++ p}) o'
                             ) xs (Right [(ys,pose)]) m
                    return m'

        vboUploadObject mtllib objmesh = do
            let (vertexlist,normallist,texcoordlist,indiceslist) = (objmesh_data objmesh) :: ([Vertex3 GLfloat],[Normal3 GLfloat],[TexCoord2 GLfloat],[Indices GLuint])

            vid <- newVBO ArrayBuffer vertexlist StaticDraw
            nid <- newVBO ArrayBuffer normallist StaticDraw
            tid <- newVBO ArrayBuffer texcoordlist StaticDraw
            iid <- newVBO ElementArrayBuffer (indicesAsList indiceslist) StaticDraw

            let gs = (IM.elems $ groups objmesh)
            ts <- sequence $ map (\g -> do
                --let is = indicesAsList $ vertex_indices g
                let offset = group_offset g
                let ni = group_numIndices g
                let mat = if isJust (group_mat g)
                           then M.findWithDefault defaultMaterial (fromJust $ group_mat g) mtllib
                           else defaultMaterial
                let trans = let (_,d) = material_dissolve mat in d < 1.0
                --iid <- newVBO ElementArrayBuffer is StaticDraw
                print offset
                print ni
                if trans
                 then return $ (Nothing,Just $ FaceGroup offset ni mat)
                 else return $ (Just $ FaceGroup offset ni mat,Nothing) ) gs

            let faces = catMaybes $ fst $ unzip ts
            let trans_faces = catMaybes $ snd $ unzip ts

            -- TODO: the position is still wrong
            return $ MeshGeometry vid (Just nid) (Just tid) Nothing iid faces trans_faces []

        vboUploadObjectPose objmesh = do
            let (vertexlist,normallist,texcoordlist,indiceslist) = (objmesh_data objmesh) :: ([Vertex3 GLfloat],[Normal3 GLfloat],[TexCoord2 GLfloat],[Indices GLuint])
            vid <- newVBO ArrayBuffer vertexlist StaticDraw
            return $ vid-}




