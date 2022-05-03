package com.example.testboy;

import javafx.application.Application;
import javafx.fxml.FXMLLoader;
import javafx.stage.Stage;

import java.io.*;
import java.util.Objects;

public class HelloApplication extends Application {
    @Override
    public void start(Stage primaryStage) throws IOException {
        //Stage stage = FXMLLoader.load(Objects.requireNonNull(getClass().getResource("simpeleAP_met_button_goed.fxml")));    // Button voorbeeld
        //Stage stage = FXMLLoader.load(Objects.requireNonNull(getClass().getResource("simpeleAP_met_label.fxml")));              // Label voorbeeld
        //Stage stage = FXMLLoader.load(Objects.requireNonNull(getClass().getResource("simpeleAP_met_entry.fxml")));              // Entry voorbeeld
        //Stage stage = FXMLLoader.load(Objects.requireNonNull(getClass().getResource("vbox_met_elementen.fxml")));
        //Stage stage = FXMLLoader.load(Objects.requireNonNull(getClass().getResource("hbox_met_elementen.fxml")));
        //Stage stage = FXMLLoader.load(Objects.requireNonNull(getClass().getResource("tabpane.fxml")));
        //Stage stage = FXMLLoader.load(Objects.requireNonNull(getClass().getResource("simpleTabPane.fxml")));
        //Stage stage = FXMLLoader.load(Objects.requireNonNull(getClass().getResource("gui_literatuur_metstage_checkButton_radioButton.fxml")));
        //Stage stage = FXMLLoader.load(Objects.requireNonNull(getClass().getResource("combobox_choicebox.fxml")));
        //Stage stage = FXMLLoader.load(Objects.requireNonNull(getClass().getResource("hyperlinkje.fxml")));
        //Stage stage = FXMLLoader.load(Objects.requireNonNull(getClass().getResource("gui_literatuur_metstage_CORRECT.fxml")));

        Stage stage = FXMLLoader.load(Objects.requireNonNull(getClass().getResource("ultiemeTest.fxml")));
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