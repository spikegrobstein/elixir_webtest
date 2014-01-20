defmodule ElixirWebtest.Dynamo do
  use Dynamo

  config :dynamo,
    # The environment this Dynamo runs on
    env: Mix.env,

    # The OTP application associated with this Dynamo
    otp_app: :elixir_webtest,

    # The endpoint to dispatch requests to
    endpoint: ApplicationRouter,

    # The route from which static assets are served
    # You can turn off static assets by setting it to false
    static_route: "/static"

  # Uncomment the lines below to enable the cookie session store
  # config :dynamo,
  #   session_store: Session.CookieStore,
  #   session_options:
  #     [ key: "_elixir_webtest_session",
  #       secret: "fn465iWyzBqSHFiBcZ/RNDoTdXegaFqryV0v8Y30X4CZ9sIUj1YbzibHgrmrIQzr"]

  # Default functionality available in templates
  templates do
    use Dynamo.Helpers
  end

  # initializer :redis do
    # Redis.start
  # end
end
