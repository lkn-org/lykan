defmodule Lykan.Repo.Migrations.CreateCharacters do
  use Ecto.Migration

  def change do
    create table(:characters) do
      add :name, :string
      add :account_id, references(:accounts, null: false)

      timestamps()
    end

    create(unique_index(:characters, [:name]))
  end
end
