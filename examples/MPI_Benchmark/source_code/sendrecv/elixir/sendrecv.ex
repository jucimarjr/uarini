defmodule SendRecv do
  @moduledoc """
  MPI Benchmark [SendRecv] in elixir by Filipe Varjão <frgv@cin.ufpe.br>
  """
 
  def run(data_size, rep, qtd_procs) do
 
    data_size = data_size
    rep =  rep
    qtd_procs = qtd_procs
 
    data = generate_data(data_size)
 
    spawn_start = time_microseg()
    second = create_procs(qtd_procs)
    spawn_end = time_microseg()
 
    exec_start = time_microseg()
    sender_ring_node(data, rep, second)
    exec_end = time_microseg()
 
    total_time = exec_end - exec_start
    spawn_time = spawn_end - spawn_start
 
    # PRINT RESULT
    IO.puts "bytes #{data_size} | repetitions #{rep} | exec_time[µsec] #{total_time} | MBytes/sec #{spawn_time} | spawn_time #{bandwidth_calc(data_size, total_time)}"
 
  end
 
  def ring_node(right_peer) do
    receive do
      data ->
        right_peer |> send(data)
        ring_node(right_peer)
    end
  end
 
  def create_procs(qtd_procs) do
    List.foldl(
      :lists.seq(qtd_procs, 2, -1),
      self,
      fn(_id, right_peer) -> spawn(__MODULE__, :ring_node, [right_peer]) end
    )
  end
 
  def sender_ring_node(_, 0, _) do
    :ok
  end
 
  def sender_ring_node(data, rep, second) do
    second |> send(data)
    receive do
      data ->
        sender_ring_node(data, rep - 1, second)
    end
  end
 
  def bandwidth_calc(data, time) do
    megabytes = (data / :math.pow(2, 20))
    seconds = time * 1.0e-6
    megabytes / seconds
  end
 
  def generate_data(size), do: generate_data(size, [])
 
  def generate_data(0, bytes), do: iolist_to_binary(bytes)
 
  def generate_data(size, bytes), do: generate_data(size - 1, [1 | bytes])
 
  def time_microseg() do
    {ms, s, us} = :erlang.now()
    (ms * 1.0e+12) + (s * 1.0e+6) + us
  end
end

