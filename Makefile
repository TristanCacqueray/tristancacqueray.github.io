PHONY = publish clean
all: publish clean
publish:
	nix build -j4
	rsync --delete --exclude .infomaniak-maintenance.html --exclude .user.ini -rvpti result/ cdn.midirus.com:sites/midirus.com/
clean:
	$(eval STORE_PATH := $(shell readlink result))
	echo "Removing: $(STORE_PATH)"
	rm result
	nix-store --delete $(STORE_PATH)
