defmodule Lykan.Repo.Character do
  use Ecto.Schema
  import Ecto.Changeset

  schema "characters" do
    field :name, :string
    belongs_to :account, Lykan.Repo.Account

    timestamps()
  end

  def validation_changeset(character) do
    character
    |> validate_required([:name])
    |> assoc_constraint(:account)
    |> unique_constraint(:name)
  end

  def new(name, of: account_id) do
    %__MODULE__{
      name: name,
    } |> cast(%{"account_id" => account_id}, [:account_id])
      |> validation_changeset()
      |> Lykan.Repo.insert()
  end
end
