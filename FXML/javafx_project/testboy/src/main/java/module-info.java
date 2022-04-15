module com.example.testboy {
    requires javafx.controls;
    requires javafx.fxml;


    opens com.example.testboy to javafx.fxml;
    exports com.example.testboy;
    exports com.example.testboy.relations;
    opens com.example.testboy.relations to javafx.fxml;
}