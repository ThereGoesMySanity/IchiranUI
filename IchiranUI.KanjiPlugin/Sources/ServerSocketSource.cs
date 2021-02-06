using System;
using System.Linq;
using System.Net;
using System.Net.Sockets;
using System.Text;
using System.Threading;
using System.Threading.Tasks;
using Avalonia.Controls;
using Avalonia.Data;
using IchiranUI.KanjiPlugin.ViewModels;
using IchiranUI.KanjiPlugin.Views;

namespace IchiranUI.KanjiPlugin.Sources
{
    public class ServerSocketSource : SocketSource
    {
        private Socket Socket;
        private SocketViewModel SocketVm;
        private Task acceptLoop;
        private CancellationTokenSource finished = new CancellationTokenSource();

        public override string Name => "Socket Server";

        public override void End()
        {
            finished.Cancel();
            try
            {
                acceptLoop.Wait();
            }
            catch (AggregateException e)
            {
                if (!(e.InnerException is TaskCanceledException))
                {
                    throw e.InnerException;
                }
            }
        }

        public override async Task Start()
        {
            SocketVm.ConnectionFailed = false;
            SocketVm.Status = "Waiting on connections";
            IPHostEntry host = Dns.GetHostEntry(Dns.GetHostName());
            IPAddress addr = host.AddressList.Where(i => i.AddressFamily == AddressFamily.InterNetwork).FirstOrDefault();
            IPEndPoint endpoint = new IPEndPoint(addr, int.Parse(SocketVm.Port));
            Socket = new Socket(addr.AddressFamily, SocketType.Stream, ProtocolType.Tcp);
            Socket.Bind(endpoint);
            Socket.Listen(1);
            try
            {
                await Socket.ConnectAsync(endpoint);
                SocketVm.Status = "Connected";
                acceptLoop = Task.Run(AcceptLoop);
            }
            catch (Exception e)
            {
                SocketVm.Status = $"Connection failed: {e}";
                SocketVm.ConnectionFailed = true;
                Socket.Close();
            }
        }

        private async Task AcceptLoop()
        {
            CancellationToken token = finished.Token;
            var reg = token.Register(Socket.Close);
            Socket client = null;
            using (reg) try
            {
                while (!token.IsCancellationRequested)
                {
                    client = await Socket.AcceptAsync();
                    await ReadLoop(client, token);
                }
            }
            catch (Exception ex) when (token.IsCancellationRequested)
            {
                throw new TaskCanceledException("AcceptLoop canceled", ex, token);
            }
            catch
            {
                throw;
            }
            finally
            {
                client?.Close();
            }
        }

        private async Task ReadLoop(Socket socket, CancellationToken token)
        {
            byte[] recvBuffer = new byte[1024];
            while (!token.IsCancellationRequested)
            {
                string text = string.Empty;
                int bytes = 0;
                while (!text.EndsWith('\n'))
                {
                    bytes = await socket.ReceiveAsync(new ArraySegment<byte>(recvBuffer), SocketFlags.None, token);
                    text += Encoding.UTF8.GetString(recvBuffer, 0, bytes);
                    if (bytes == 0)
                    {
                        finished.Cancel();
                        SocketVm.Status = "Error: Disconnected";
                        SocketVm.ConnectionFailed = true;
                        break;
                    }
                }
                AddSentences(text);
            }
        }
    }
}
