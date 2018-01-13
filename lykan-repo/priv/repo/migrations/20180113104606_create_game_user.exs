defmodule Lykan.Repo.Migrations.CreateGameUser do
  use Ecto.Migration

  defp user() do
    "lykan_game_dev"
  end

  defp table() do
    table = Keyword.get(Application.get_env(:lykan_repo, Lykan.Repo), :database, "lykan")
  end

  def up do
    execute "CREATE USER #{user()} WITH NOCREATEDB;"
    execute "GRANT CONNECT ON DATABASE #{table()} TO #{user()};"
    execute "GRANT SELECT ON accounts TO #{user()};"
    execute "GRANT SELECT, INSERT, UPDATE, DELETE ON characters TO #{user()};"
  end

  def down do
    execute "DROP OWNED BY #{user()}";
    execute "DROP USER #{user()};";
  end
end
