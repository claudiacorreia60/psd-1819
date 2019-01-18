package peerLending;

import org.zeromq.ZMQ;


public class Publisher {
    ZMQ.Context context;
    ZMQ.Socket publisher;


    public Publisher (ZMQ.Context context, ZMQ.Socket publisher) {
        this.context = context;
        this.publisher = publisher;
    }

    public void sendNotification (String notification) {
        this.publisher.send(notification);
    }
}
