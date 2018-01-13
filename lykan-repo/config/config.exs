use Mix.Config

config :lykan_repo,
  ecto_repos: [
    Lykan.Repo,
  ]

config :lykan_repo, Lykan.Repo,
  adapter: Ecto.Adapters.Postgres,
  database: "lykan_dev",
  username: "lykan_dev",
  password: "",
  hostname: "localhost"
