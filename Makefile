PHONY = publish clean sync-cdn

publish:
	nix build -j4
	rsync --delete --exclude .infomaniak-maintenance.html --exclude .user.ini -rvpti result/ cdn.midirus.com:sites/midirus.com/
clean:
	$(eval STORE_PATH := $(shell readlink result))
	echo "Removing: $(STORE_PATH)"
	rm result
	nix-store --delete $(STORE_PATH)

sync-cdn:
	rsync --delete -rvpti /srv/cdn.midirus.com/ cdn.midirus.com:sites/cdn.midirus.com/
