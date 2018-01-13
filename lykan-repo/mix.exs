defmodule Lykan.Repo.Mixfile do
  use Mix.Project

  def project do
    [
      app:      :lykan_repo,
      version:  "0.1.0",
      elixir:   "~> 1.5",
      start_permanent: Mix.env == :prod,
      deps:     deps(),
    ]
  end

  def application do
    [
      extra_applications: [
        :logger,
      ],
    ]
  end

  defp deps do
    [
      {:ecto,          "~> 2.1"},
      {:postgrex,      "~> 0.13"},
      {:lkn_prelude,   "~> 0.1"},
    ]
  end
end
