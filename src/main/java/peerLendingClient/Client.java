package peerLendingClient;

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

    public String getUsername() { return this.username; }

    public void setUsername(String username) { this.username = username; }

    public String getPassword() { return this.password; }

    public void setPassword(String password) { this.password = password; }


    public void startClient() throws IOException {
        System.out.println("> Connecting to server...");
        Socket socket = new Socket(this.hostname, this.port);
        System.out.println("> Connection accepted!");

        InputStream in = socket.getInputStream();
        OutputStream out = socket.getOutputStream();
        BufferedReader reader = new BufferedReader(new InputStreamReader(System.in));

        System.out.println("\n######### LOGIN #########");
        System.out.print("\n> Username: ");
        this.username = reader.readLine();
        System.out.print("> Password: ");
        this.password = reader.readLine();
        ClientProtos.Authentication auth = ClientProtos.Authentication.newBuilder()
                .setUsername(this.username)
                .setPassword(this.password)
                .build();

        out.write(auth.toByteArray());
        out.flush();
        byte [] response = this.recv(in);
        ClientProtos.Result ans = ClientProtos.Result.parseFrom(response);

        boolean result = ans.getResult();
        String entity = ans.getEntity();
        if (!result)
            System.out.println("ERROR: Authentication failed!");
        else if (entity.equals("investor")) {
            Investor investor = new Investor(username, password, in, out, reader);
            investor.handleInvestor();
        }
        else {
            Company company = new Company(username, password, in, out, reader);
            company.handleCompany();
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
        } catch (IOException e) {
            e.printStackTrace();
        }
        return null;
    }

    public static void main (String[] args) throws IOException {
        Client c = new Client("192.168.1.146", 1231);
        c.startClient();
    }
}
