
import Network.N2O.Proto

main :: IO ()
main = sequence_ [test m | m <- msgs]

protos :: [ProtoBox]
protos = [ProtoBox N2OClientProto, ProtoBox N2OIoProto]

msgs :: [Term]
msgs = [ TupleTerm [AtomTerm "client", AtomTerm "hello"]
        , TupleTerm [AtomTerm "io", NilTerm, NilTerm]
        ]

test :: Term -> IO ()
test m = forM_ protos (\(ProtoBox proto) ->
    case (fromBert m) of
        Nothing -> return ()
        Just x -> info proto x)

-- > main
-- n2o client proto: TupleTerm [AtomTerm "client",AtomTerm "hello"]
-- n2o io message: TupleTerm [AtomTerm "io",NilTerm,NilTerm]
