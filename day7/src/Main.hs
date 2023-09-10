type Name = String
type Size = Int

data Path = Directory Name [Path] | File Name Size

main :: IO ()
main = print "Hi"
