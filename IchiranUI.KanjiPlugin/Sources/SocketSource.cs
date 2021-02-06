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
    public class SocketSource : Source
    {
        private Socket Socket;
        private SocketViewModel SocketVm;
        private Task readLoop;
        private CancellationTokenSource finished = new CancellationTokenSource();

        public SocketSource()
        {
            SocketVm = new SocketViewModel(this);
            SettingsPage = new SocketSettingsView
            {
                DataContext = SocketVm,
            };
            ControlsPage = new SocketRunView
            {
                DataContext = SocketVm,
            };
        }

        public override Control SettingsPage { get; } 

        public override Control ControlsPage { get; }

        public override string Name => "Socket Client";

        public override void End()
        {
            finished.Cancel();
            try
            {
                readLoop.Wait();
            }
            catch (AggregateException e)
            {
                if (!(e.InnerException is TaskCanceledException))
                {
                    throw e.InnerException;
                }
            }
            Socket.Close();
        }

        public void Clear()
        {
            Sentences.Clear();
        }

        public async Task Reconnect()
        {
            await Start();
        }

        public override async Task Start()
        {
            SocketVm.ConnectionFailed = false;
            SocketVm.Status = "Connecting...";
            IPHostEntry host = Dns.GetHostEntry(SocketVm.IpAddress);
            IPAddress addr = host.AddressList.Where(i => i.AddressFamily == AddressFamily.InterNetwork).FirstOrDefault();
            IPEndPoint endpoint = new IPEndPoint(addr, int.Parse(SocketVm.Port));
            Socket = new Socket(addr.AddressFamily, SocketType.Stream, ProtocolType.Tcp);
            try
            {
                await Socket.ConnectAsync(endpoint);
                SocketVm.Status = "Connected";
                readLoop = Task.Run(ReadLoop);
            }
            catch (Exception e)
            {
                SocketVm.Status = $"Connection failed: {e}";
                SocketVm.ConnectionFailed = true;
                Socket.Close();
            }
        }

        private async Task ReadLoop()
        {
            CancellationToken token = finished.Token;
            byte[] recvBuffer = new byte[1024];
            while (!token.IsCancellationRequested)
            {
                string text = string.Empty;
                int bytes = 0;
                while (!text.EndsWith('\n'))
                {
                    bytes = await Socket.ReceiveAsync(new ArraySegment<byte>(recvBuffer), SocketFlags.None, token);
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
