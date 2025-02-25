## Resubmission

This is a resubmission. 
- The agricultural bulletin services was removed by one of the providers (Bureau of Meteorology Australia) which led to breaking changes.
- Fix error when re-building vignette outputs
- Adhere to CRAN policy to fail gracefully when using internet resources (.get_url() internal function)
- Cross platform CMD checks are performed via GitHub Actions for: macos-release, windows-release, ubuntu-devel, ubuntu-release and ubuntu-oldrel-1.

## R CMD check results

0 errors | 0 warnings | 1 note

## URL Check Note

Several URLs are reported as "Forbidden".
This is due to an Australian Bureau of Meteorology policy that blocks any headless access of web resources that are served via HTTP methods but if accessed via a browser, properly respond.
The URLs have all been validated and are considered to be stable and unchanging as a part of a government website.

