[![Build Status](https://travis-ci.org/barakb/FilesCache.svg?branch=master)](https://github.com/barakb/FilesCache)

# FilesCache

Proxy to large zip files located on remote HTTP server.
It use local folder as cache to the last requested N files. 
Cache is manage as logical LRU where each access increment an LMT field inside the LRU cache 
for the given entry.

When there are too many entries the least used are deleted from the cache and the disk.
The proxy will unite requests for a file that in the process of download (no multiple download window)
It does not (for now) serve the first request in pipe as it download, 
first it will download the file to the cache and only after that it will serve the file to each requester.
The file upload/download is however done in streaming (fix memory)

