production:
	stack --docker build --force-dirty --ghc-options -O2 --ghc-options -threaded
	cp .stack-work/install/x86_64-linux/lts-3.15/7.10.2/bin/haskellbr-website dist/bin/haskellbr-website
	upx dist/bin/haskellbr-website
	tar -czvf haskellbr-website.tar.gz config/keter.yml static dist/bin/haskellbr-website

clean:
	rm {**/,}*.{o,hi} || echo ">>> No files to remove"

deploy: production
	scp haskellbr-website.tar.gz haskellbr@box:/home/haskellbr/website/
	ssh haskellbr@box -- "cd /home/haskellbr/website && tar xzfv haskellbr-website.tar.gz && service haskellbr-website restart"
