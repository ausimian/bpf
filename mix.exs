defmodule BPF.MixProject do
  use Mix.Project

  @version "0.2.0"
  @source_url "https://github.com/ausimian/bpf"

  def project do
    [
      app: :bpf,
      version: @version,
      elixir: "~> 1.15",
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      aliases: aliases(),
      package: package(),
      description: description(),

      # Test coverage
      test_coverage: [tool: ExCoveralls],
      preferred_cli_env: [
        coveralls: :test,
        "coveralls.detail": :test,
        "coveralls.post": :test,
        "coveralls.html": :test,
        "coveralls.github": :test
      ],

      # Docs
      name: "BPF",
      source_url: @source_url,
      homepage_url: @source_url,
      docs: [
        main: "BPF",
        extras: ["README.md", "CHANGELOG.md", "LICENSE.md"],
        source_ref: @version
      ]
    ]
  end

  defp description do
    """
    Compile Elixir binary pattern matching to classic BPF bytecode.
    """
  end

  defp package do
    [
      name: "bpf",
      licenses: ["MIT"],
      links: %{
        "GitHub" => @source_url
      },
      files: ~w(lib .formatter.exs mix.exs README.md CHANGELOG.md LICENSE.md)
    ]
  end

  def aliases do
    [
      "expublish.major": &expublish("expublish.major", &1),
      "expublish.minor": &expublish("expublish.minor", &1),
      "expublish.patch": &expublish("expublish.patch", &1),
      "expublish.stable": &expublish("expublish.stable", &1),
      "expublish.rc": &expublish("expublish.rc", &1),
      "expublish.beta": &expublish("expublish.beta", &1),
      "expublish.alpha": &expublish("expublish.alpha", &1)
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:logger]
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:ex_doc, "~> 0.39", only: :dev, runtime: false},
      {:expublish, "~> 2.5", only: [:dev], runtime: false},
      {:excoveralls, "~> 0.18", only: :test},
      {:castore, "~> 1.0", only: :test}
    ]
  end

  defp expublish(task, args) do
    common = ["--tag-prefix", "", "--commit-prefix", "Version", "--branch", ""]

    if "--no-dry-run" in args do
      Mix.Task.run(task, common ++ args)
    else
      Mix.Task.run(task, ["--dry-run" | common] ++ args)
    end
  end
end
