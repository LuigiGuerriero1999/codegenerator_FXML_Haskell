package com.example.testboy.structures;

public class Label extends GTKWidget{
    private String text;
    private String labelName;
    private double layoutX;
    private double layoutY;

    private double width;
    private double height;

    public Label(String text, double layoutX, double layoutY, double width, double height) {
        this.text = text;
        this.layoutX = layoutX;
        this.layoutY = layoutY;
        this.width = width;
        this.height = height;
    }

    public Label(String id, Integer id_hash, String name, String text, double layoutX, double layoutY, double width, double height) {
        super(id, id_hash, makeName(id,id_hash,name)+"Container");
        this.text = text;
        this.layoutX = layoutX;
        this.layoutY = layoutY;
        this.width = width;
        this.height = height;
        this.labelName = makeName(id,id_hash,name);
    }

    public String getText() {
        return text;
    }

    public void setText(String text) {
        this.text = text;
    }

    public double getLayoutX() {
        return layoutX;
    }

    public void setLayoutX(double layoutX) {
        this.layoutX = layoutX;
    }

    public double getLayoutY() {
        return layoutY;
    }

    public void setLayoutY(double layoutY) {
        this.layoutY = layoutY;
    }

    public double getWidth() {
        return width;
    }

    public void setWidth(double width) {
        this.width = width;
    }

    public double getHeight() {
        return height;
    }

    public void setHeight(double height) {
        this.height = height;
    }

    public String gtkHsCode(){
        String labelGtkHsCode = labelName + " <- Gtk.labelNew (Just " + "\"" + text + "\"" + ")\n  ";
        return labelGtkHsCode;
    }
}
