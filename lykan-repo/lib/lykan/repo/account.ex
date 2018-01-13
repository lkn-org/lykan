use Lkn.Prelude

defmodule Lykan.Repo.Account do
  use Ecto.Schema
  import Ecto.{Changeset, Query}

  schema "accounts" do
    field :email, :string
    has_many :characters, Lykan.Repo.Character

    timestamps()
  end

  @required_fields [:email]

  def validation_changeset(account) do
    account
    |> validate_required(@required_fields)
    |> unique_constraint(:email)
  end

  def new(email) do
    %__MODULE__{
      email: email
    } |> cast(%{}, [])
      |> validation_changeset()
      |> Lykan.Repo.insert()
  end

  def with_email(email, opts \\ []) do
    preload = if Keyword.get(opts, :preload, false), do: [:characters], else: []

    query = from(
      account in Lykan.Repo.Account,
      where: account.email == ^email,
      select: account,
      preload: ^preload
    )

    case Lykan.Repo.all(query) do
      [res] -> Option.some(res)
      _     -> Option.nothing()
    end
  end
end
