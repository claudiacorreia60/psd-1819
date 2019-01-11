package peerLendingClient;

import org.zeromq.ZMQ;
import peerLending.Company;
import peerLending.Investor;
import java.io.*;
import java.net.Socket;


public class Client {
    private String username;
    private String password;
    private String hostname;
    private int port;


    public Client(String hostname, int port) {
        this.hostname = hostname;
        this.port = port;
    }

    public String getUsername() {
        return this.username;
    }

    public void setUsername(String username) {
        this.username = username;
    }

    public String getPassword() {
        return this.password;
    }

    public void setPassword(String password) {
        this.password = password;
    }


    public void startClient() throws IOException {
        System.out.println("> Connecting to server...");
        Socket socket = new Socket(this.hostname, this.port);
        System.out.println("> Connection accepted!");

        InputStream in = socket.getInputStream();
        OutputStream out = socket.getOutputStream();
        BufferedReader reader = new BufferedReader(new InputStreamReader(System.in));

        System.out.println("\n################ LOGIN ################");
        System.out.print("\n> Username: ");
        this.username = reader.readLine();
        System.out.print("> Password: ");
        this.password = reader.readLine();
        ClientProtos.Authentication auth = ClientProtos.Authentication.newBuilder()
                .setUsername(this.username)
                .setPassword(this.password)
                .build();

        ClientProtos.Message msg = ClientProtos.Message.newBuilder()
                .setAuth(auth)
                .build();

        out.write(msg.toByteArray());
        out.flush();

        byte [] response = this.recv(in);
        ClientProtos.Message ans = ClientProtos.Message.parseFrom(response);
        ClientProtos.Result res = ans.getRes();

        boolean result = res.getResult();
        String entity = res.getEntity();
        //System.out.println("RESULT: "+result+"\nENTITY: "+entity);
        if (!result)
            System.out.println("ERROR: Authentication failed!");
        else {
            ZMQ.Context context = ZMQ.context(1);
            ZMQ.Socket subscriber = context.socket(ZMQ.SUB);
            Subscriber sub = new Subscriber(context, subscriber);
            Thread t = new Thread(sub);
            t.start();
            if (entity.equals("investor")) {
                Investor investor = new Investor(this.username, this.password, in, out, reader, subscriber);
                investor.handleInvestor();
            }
            else {
                Company company = new Company(this.username, this.password, in, out, reader, subscriber);
                company.handleCompany();
            }
        }
    }

    public byte[] recv(InputStream in){
        byte[] tmp = new byte[4096];
        int len = 0;
        try {
            len = in.read(tmp);
            byte[] response = new byte[len];

            for(int i = 0; i < len; i++)
                response[i] = tmp[i];
            return response;
        }
        catch (IOException e) {
            e.printStackTrace();
        }
        return null;
    }

    public static void main (String[] args) throws IOException {
        Client c = new Client("192.168.1.146", 3000);
        c.startClient();
    }
}
