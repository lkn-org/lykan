use Mix.Config

config :lykand,
  ecto_repos: [Lykan.Repo],
  game_port: 4000,
  auth_port: 4001

config :lykan_repo, Lykan.Repo,
  adapter: Ecto.Adapters.Postgres,
  database: "lykan_dev",
  username: "lykan_game_dev",
  password: "",
  hostname: "localhost"
