package com.example.testboy.structures;

public class Label {
    private String name;

    private String text;

    private double x;
    private double y;

    private double width;
    private double height;

    public Label(String name, String text, double x, double y, double width, double height) {
        this.name = name;
        this.text = text;
        this.x = x;
        this.y = y;
        this.width = width;
        this.height = height;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getText() {
        return text;
    }

    public void setText(String text) {
        this.text = text;
    }

    public double getX() {
        return x;
    }

    public void setX(double x) {
        this.x = x;
    }

    public double getY() {
        return y;
    }

    public void setY(double y) {
        this.y = y;
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

        String labelGtkHsCode = name + " <- Gtk.labelNew (Just " + "\"" + text + "\"" + ") \n";

        return labelGtkHsCode;
    }
}
