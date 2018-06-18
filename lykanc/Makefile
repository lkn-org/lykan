run:
	@sbcl --eval "(ql:quickload :lykanc)" \
              --eval "(lykanc:run)"

bundle:
	@echo -en "[ ] Remove old build"
	@rm -rf build/
	@echo -e "\r[*] Remove old build"
	@echo "[ ] Building"
	@sbcl --eval "(ql:quickload :lykanc/bundle)" --eval "(lykanc/bundle:deliver)" --quit
	@echo "[*] Building"

.PHONY: bundle run
