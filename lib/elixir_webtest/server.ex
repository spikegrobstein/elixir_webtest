defmodule ElixirWebtest.Server do
  use GenServer.Behaviour

  def start_link(users) do
    :gen_server.start_link({:local, :userstore}, __MODULE__, users, [])
  end

  def init(users) do
    {:ok, users}
  end

  def handle_call( :list_users, _from, users ) do
    { :reply, users, users }
  end

  def handle_cast( { :add_user, new_user }, users ) do
    IO.puts "broadcasting add..."

    :gen_server.cast( :subscriber_store, { :broadcast, { :add, new_user } } )

    { :noreply, [ new_user | users ] }
  end

end
