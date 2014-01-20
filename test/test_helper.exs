Dynamo.under_test(ElixirWebtest.Dynamo)
Dynamo.Loader.enable
ExUnit.start

defmodule ElixirWebtest.TestCase do
  use ExUnit.CaseTemplate

  # Enable code reloading on test cases
  setup do
    Dynamo.Loader.enable
    :ok
  end
end
