.PHONY: server migrate

server: migrate
	@echo [*] update dependencies
	@cd lykand; mix deps.get
	@echo [*] run lykand
	@cd lykand; iex -S mix

migrate:
	@echo [*] run ecto migrations
	@cd lykan-repo; mix ecto.create
	@cd lykan-repo; mix ecto.migrate
