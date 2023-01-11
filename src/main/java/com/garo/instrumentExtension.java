// package com.garo;

// import java.util.ArrayList;
// import java.util.List;

// import com.bitwig.extension.api.util.midi.ShortMidiMessage;
// import com.bitwig.extension.callback.ShortMidiMessageReceivedCallback;
// import com.bitwig.extension.controller.api.ControllerHost;
// import com.bitwig.extension.controller.api.Transport;
// import com.bitwig.extension.controller.ControllerExtension;

// public class instrumentExtension extends ControllerExtension {
//    protected instrumentExtension(final instrumentExtensionDefinition definition, final ControllerHost host) {
//       super(definition, host);
//    }

//    @Override
//    public void init() {
//       final ControllerHost host = getHost();

//       mTransport = host.createTransport();
//       host.getMidiInPort(0).setMidiCallback((ShortMidiMessageReceivedCallback) msg -> onMidi(msg));

//       // TODO: Perform your driver initialization here.
//       // For now just show a popup notification for verification that it is running.
//       host.showPopupNotification("instrument Initialized");
//    }

//    @Override
//    public void exit() {
//       // TODO: Perform any cleanup once the driver exits
//       // For now just show a popup notification for verification that it is no longer
//       // running.
//       getHost().showPopupNotification("instrument Exited");
//    }

//    @Override
//    public void flush() {
//       // TODO Send any updates you need here.
//    }

//    /** Called when we receive short MIDI message on port 0. */

//    private List<Integer> keyState = new ArrayList<>(128);
//    private void onMidi(ShortMidiMessage msg) {
//       final List<Integer> oldState = keyState;
//       keyState = new ArrayList<>(oldState);
//       if (msg.isNoteOn()) {
//          keyState.set(msg.getData1(), 1);
//       } else if (msg.isNoteOff()) {
//          keyState.set(msg.getData1(), 0);
//       }

//       controlStep(oldState, keyState);
//    }

//    private void controlStep(List<Integer> oldState, List<Integer> newState) {}

//    private Transport mTransport;
// }
