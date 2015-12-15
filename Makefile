production:
	stack --docker build --ghc-options -O2 --ghc-options -threaded
	cp `stack --docker path --local-install-root`/bin/haskellbr-website dist/bin/haskellbr-website
	upx dist/bin/haskellbr-website
	tar -czvf haskellbr-website.tar.gz config/keter.yml static dist/bin/haskellbr-website

docker:
	@echo "\n>>> Building docker image assets...\n"
	mkdir -p docker-assets/{dist/bin,config}
	stack --docker build --ghc-options -O2 --ghc-options -threaded
	cp `stack --docker path --dist-dir`/build/haskellbr-website/haskellbr-website docker-assets/dist/bin/haskellbr-website
	cp -r static docker-assets/
	cp config/keter.yml docker-assets/config/
	@echo "\n>>> Compressing executable...\n"
	upx docker-assets/dist/bin/haskellbr-website
	@echo "\n>>> Building docker image for current git revision..."
	@echo "      \---> Image will be tagged as haskellbr/website:`git rev-parse HEAD`\n"
	docker build -t haskellbr/website:`git rev-parse HEAD` docker-assets

docker-push:
	docker push haskellbr/website:`git rev-parse HEAD`

clean:
	rm {**/,}*.{o,hi} || echo ">>> No files to remove"

deploy: production
	scp haskellbr-website.tar.gz haskellbr@box:/home/haskellbr/website/
	ssh haskellbr@box -- "cd /home/haskellbr/website && tar xzfv haskellbr-website.tar.gz && service haskellbr-website restart"
