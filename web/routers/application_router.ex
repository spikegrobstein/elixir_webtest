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

    event_handler conn
  end

  get "/api/login/:name" do
    # Redis.sadd("users", conn.params[:name])
    :gen_server.cast( :userstore, { :add_user, conn.params[:name] } )

    redirect conn, to: "/users"
  end

  get "/api/logout/:name" do
    :gen_server.cast( :userstore, { :del_user, conn.params[:name] } )

    redirect conn, to: "/users"
  end

  defp event_handler( conn ) do
    result = await( conn, 5000, &handle_event(&1, &2), &on_time_out(&1) )

    case result do
      { :timeout } ->
        # disconnect the client!
        # conn
        event_handler( conn )
      { :ok, _ } ->
        event_handler( conn )
      { :error, :closed } ->
        # my event stream connection closed
        # so delete self from the subscriberstore and terminate
        :gen_server.cast( :subscriber_store, { :del, self } )
        conn
      _ ->
        event_handler( conn )
    end

  end

  defp handle_event( { :add, user }, conn ) do
    send_chunk conn, [ action: "add", user: user ]
  end

  defp handle_event( { :del, user }, conn ) do
    send_chunk conn, [ action: "del", user: user ]
  end

  defp handle_event( msg, _conn ) do
    msg
  end

  defp on_time_out( _a ) do
    { :timeout }
  end

  defp send_chunk( conn, data ) do
    result = JSON.encode(data)

    case result do
      { :ok, json } ->
        conn.chunk "data: #{ json }\n\n"
      _ ->
        conn
    end
  end

end
