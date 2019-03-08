webmiddens
==========




[![Project Status: WIP – Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)

simple caching of HTTP requests/responses, hooking into [webmockr](https://github.com/ropensci/webmockr) 
for the HTTP request matching

### the need

- `vcr` is meant really for testing, or sort of script use. i don't think it fits
well into a use case where another pkg wants to cache responses
- `memoise` seems close-ish but doesn't fit needs, e.g., no expiry, not specific
to HTTP requests, etc.
- we need something specific to HTTP requests, that allows easy expiration handling,
a few different caching location options, works across HTTP clients, xxx

### brainstorming

- use `webmockr` to match requests (works with `crul` and `httr`, maybe `curl` soon)
- possibly match on, and expire based on headers: Cache-Control, Age, Last-Modified,
ETag, Expires (see [ruby's [faraday-http-cache](https://github.com/plataformatec/faraday-http-cache#what-gets-cached)])
- caching backends: probably all binary to save disk space since most likely
we don't need users to be able to look at plain text of caches
- expiration: set a time to expire. if set to `2019-03-08 00:00:00` and it's
`2019-03-07 23:00:00`, then 1 hr from now the cache will expire, and a new real HTTP 
request will need to be made (i.e., the cache will be deleted)

### how though?


```r
library(webmiddens)
library(crul)
con <- crul::HttpClient$new("https://httpbin.org")
```

without webmiddens


```r
con$get(query = list(stuff = "bananas"))
#> <crul response> 
#>   url: https://httpbin.org/?stuff=bananas
#>   request_headers: 
#>     User-Agent: libcurl/7.54.0 r-curl/3.3 crul/0.7.0
#>     Accept-Encoding: gzip, deflate
#>     Accept: application/json, text/xml, application/xml, */*
#>   response_headers: 
#>     status: HTTP/1.1 200 OK
#>     access-control-allow-credentials: true
#>     access-control-allow-origin: *
#>     content-encoding: gzip
#>     content-type: text/html; charset=utf-8
#>     date: Fri, 08 Mar 2019 19:17:54 GMT
#>     server: nginx
#>     content-length: 3168
#>     connection: keep-alive
#>   params: 
#>     stuff: bananas
#>   status: 200
```

with webmiddens, make a midden object first


```r
x <- midden$new()
x$init(path = "rainforest")
```



first request is a real HTTP request


```r
x$call(con$get("get", query = list(stuff = "bananas")))
#> <crul response> 
#>   url: https://httpbin.org/get?stuff=bananas
#>   request_headers: 
#>     User-Agent: libcurl/7.54.0 r-curl/3.3 crul/0.7.0
#>     Accept-Encoding: gzip, deflate
#>     Accept: application/json, text/xml, application/xml, */*
#>   response_headers: 
#>     status: HTTP/1.1 200 OK
#>     access-control-allow-credentials: true
#>     access-control-allow-origin: *
#>     content-encoding: gzip
#>     content-type: application/json
#>     date: Fri, 08 Mar 2019 19:17:55 GMT
#>     server: nginx
#>     content-length: 236
#>     connection: keep-alive
#>   params: 
#>     stuff: bananas
#>   status: 200
```

second request uses the cached response from the first request


```r
x$call(con$get("get", query = list(stuff = "bananas")))
#> <crul response> 
#>   url: https://httpbin.org/get?stuff=bananas
#>   request_headers: 
#>     User-Agent: libcurl/7.54.0 r-curl/3.3 crul/0.7.0
#>     Accept-Encoding: gzip, deflate
#>     Accept: application/json, text/xml, application/xml, */*
#>   response_headers: 
#>   params: 
#>     stuff: bananas
#>   status: 200
```

list cached items


```r
x$cache$list()
#> [1] "/Users/sckott/Library/Caches/R/rainforest/_middens10b7842b5f098"
# & cleanup
x$destroy()
```

## Meta

* Please [report any issues or bugs](https://github.com/ropensci/webmiddens/issues).
* License: MIT
* Get citation information for `webmiddens` in R doing `citation(package = 'webmiddens')`
* Please note that this project is released with a [Contributor Code of Conduct](CODE_OF_CONDUCT.md).
By participating in this project you agree to abide by its terms.

[![ropensci_footer](https://ropensci.org/public_images/github_footer.png)](https://ropensci.org)
