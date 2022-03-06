module com.example.testboy {
    requires javafx.controls;
    requires javafx.fxml;


    opens com.example.testboy to javafx.fxml;
    exports com.example.testboy;
}