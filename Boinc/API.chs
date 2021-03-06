{-# LANGUAGE ForeignFunctionInterface #-}
module Boinc.API (boincInit,boincResolveFilename,boincFinish) where

import Foreign
import Foreign.C
import Foreign.C.Types

-- {#context lib="boinc_api"#}
#include <BOINC/boinc_api.h>

boincInit :: IO CInt
boincInit = {#call unsafe boinc_init#}

_boincFinish :: CInt -> IO CInt
_boincFinish = {#call unsafe boinc_finish#}

boincFinish :: Int -> IO CInt
boincFinish i = _boincFinish (fromIntegral i)

_boincResolveFilename :: (Ptr CChar) -> (Ptr CChar) -> CInt -> IO CInt
_boincResolveFilename = {#call unsafe boinc_resolve_filename#}

boincResolveFilename :: String -> IO String
boincResolveFilename s = withCString s (\cstr -> do
    let buflen = 8 * 512
    buf <- mallocBytes buflen
    r <- _boincResolveFilename cstr buf (fromIntegral buflen)
    val <- peekCStringLen (buf, buflen)
    free buf
    return val)
