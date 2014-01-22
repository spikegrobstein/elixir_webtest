defmodule ElixirWebtest.Supervisor do
  use Supervisor.Behaviour

  def start_link( user_list ) do
    :supervisor.start_link(__MODULE__, user_list )
  end

  def init( user_list ) do
    children = [
      worker(ElixirWebtest.SubscriberStore, [[]]),
      worker(ElixirWebtest.UserStore, [ user_list ]),
      supervisor(ElixirWebtest.Dynamo, [])
    ]

    supervise children, strategy: :one_for_one
  end
end
