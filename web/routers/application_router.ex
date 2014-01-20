defmodule ApplicationRouter do
  use Dynamo.Router

  prepare do
    # Pick which parts of the request you want to fetch
    # You can comment the line below if you don't need
    # any of them or move them to a forwarded router
    conn.fetch([:cookies, :params])
  end

  # It is common to break your Dynamo into many
  # routers, forwarding the requests between them:
  # forward "/posts", to: PostsRouter

  get "/" do
    redirect conn, to: "/users"
  end

  get "/users" do
    users = :gen_server.call( :userstore, :list_users )

    conn = conn.assign(:users, users)

    render conn, "users.html"
  end

  get "/user-stream" do
    conn = conn.resp_content_type("text/event-stream")
    conn = conn.send_chunked(200)

    # add that handler to the subscribers
    :gen_server.cast( :subscriber_store, { :add, self } )

    conn.chunk('test: testing')

    event_sender( conn )
  end

  get "/api/login/:name" do
    # Redis.sadd("users", conn.params[:name])
    :gen_server.cast( :userstore, { :add_user, conn.params[:name] } )

    redirect conn, to: "/users"
  end

  get "/api/logout/:name" do
    Redis.srem("users", conn.params[:name])

    redirect conn, to: "/users"
  end

  defp event_sender( conn ) do
    receive do
      { :add, user } ->
        IO.puts "GOT ADD USER: #{ user }"
        { :ok, conn } = conn.chunk "add: #{ user }"
       data ->
        IO.puts "ERROR in event_sender #{ inspect data }"
    end

    event_sender( conn )
  end

end
