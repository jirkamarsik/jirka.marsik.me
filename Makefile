COMMIT=$(shell git log -1 HEAD --pretty=format:%H)
SHA=$(shell echo $(COMMIT) | head --bytes=8)

_site: site index.html templates/* posts/* pages/* css/* images/*
	./site rebuild

site: site.hs
	ghc --make site.hs

check:
	./site check

clean:
	./site clean


deploy-to-loria: _site
	rsync --archive --verbose \
          _site/* \
          jmarsik@loria.loria.fr:/local/web-homepages/jmarsik

git-deploy/.git:
	rm --recursive --force git-deploy
	git clone git@github.com:jirkamarsik/jirkamarsik.github.io.git git-deploy

deploy-to-github: _site git-deploy/.git
	rm --recursive git-deploy/*
	cp --recursive _site/* git-deploy
	cd git-deploy && \
	  git add . && \
	  (git commit --all --message "Generated from $(SHA)" || true) && \
	  git push --force

deploy: deploy-to-loria deploy-to-github


.PHONY: check clean deploy-to-loria deploy-to-github deploy
