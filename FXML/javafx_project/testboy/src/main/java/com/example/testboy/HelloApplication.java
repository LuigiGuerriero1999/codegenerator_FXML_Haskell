package com.example.testboy;

import javafx.application.Application;
import javafx.fxml.FXMLLoader;
import javafx.stage.Stage;

import java.io.*;
import java.util.Objects;

public class HelloApplication extends Application {
    @Override
    public void start(Stage primaryStage) throws IOException {
        //stage = FXMLLoader.load(getClass().getResource("simpeleAP_met_button_goed.fxml"));    // Button voorbeeld
        //stage = FXMLLoader.load(getClass().getResource("simpeleAP_met_label.fxml"));              // Label voorbeeld
        //stage = FXMLLoader.load(getClass().getResource("simpeleAP_met_entry.fxml"));              // Entry voorbeeld
        Stage stage = FXMLLoader.load(Objects.requireNonNull(getClass().getResource("gui_literatuur_metstage_CORRECT.fxml")));
        stage.show();

        new GtkHaskellCode(stage);

        GtkHaskellCode.generateHaskellCode();
        GtkHaskellCode.dump(stage.getScene().getRoot());
        GtkHaskellCode.endOfFile();
    }

    public static void main(String[] args) {
        launch();
    }

}