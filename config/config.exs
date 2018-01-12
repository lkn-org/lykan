use Mix.Config

config :lykan, ecto_repos: [Lykan.Repo]

config :lykan_repo, Lykan.Repo,
  adapter: Sqlite.Ecto2,
  database: "lykan.sqlite3"
