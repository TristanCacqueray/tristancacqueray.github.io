PHONY = publish clean sync-cdn

publish:
	nix run .#build
	rsync --delete --exclude .infomaniak-maintenance.html --exclude .user.ini -rvpti _out/ cdn.midirus.com:sites/midirus.com/

sync-cdn:
	rsync --exclude /inbox/ --delete -rvpti /srv/cdn.midirus.com/ cdn.midirus.com:sites/cdn.midirus.com/
