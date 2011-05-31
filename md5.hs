import System.IO

import MD5

main = hSetEncoding stdin latin1 >> getContents >>= return . md5 >>= putStrLn
