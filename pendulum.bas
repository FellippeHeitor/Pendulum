CONST true = -1, false = 0

_TITLE "I came in like a wreeeeeecking ball..."

SCREEN _NEWIMAGE(800, 600, 32)
g = .4
ball.damping = .995
ball.radius = 30
ball.velocity = 0
ball.acceleration = 0
ball.origin.X = _WIDTH / 2
ball.origin.Y = 0
ball.x = 0
ball.y = 0

DO
    DO
        ball.acceleration = ball.acceleration + g / 10
        ball.velocity = ball.velocity + ball.acceleration
        ball.y = ball.y + ball.velocity

        IF ball.y - ball.radius / 2 > _HEIGHT THEN
            ball.y = 0
            ball.acceleration = 0
            ball.velocity = 0
        END IF

        LINE (0, 0)-(_WIDTH, _HEIGHT), _RGBA32(0, 0, 0, 170), BF

        CircleFill ball.x, ball.y, ball.radius, _RGB32(255, 255, 255)
        LINE (0, ball.y)-STEP(10, 0), _RGB32(0, 255, 0)
        LINE (ball.x, _HEIGHT)-STEP(0, -10), _RGB32(0, 255, 0)

        _DISPLAY
        _LIMIT 60
    LOOP UNTIL _KEYHIT = 13

    ball.velocity = 0
    ball.acceleration = 0
    diff.y = ball.origin.Y - ball.y
    diff.x = ball.origin.X - ball.x
    ball.angle = _ATAN2(-1 * diff.y, diff.x) - _D2R(90)
    ball.arm = dist(ball.x, ball.y, ball.origin.X, ball.origin.Y)

    DO
        WHILE _MOUSEINPUT: WEND
        mouseX = _MOUSEX
        mouseY = _MOUSEY

        IF _MOUSEBUTTON(1) THEN
            IF NOT mouseDown THEN
                mouseDown = true
                IF dist(mouseX, mouseY, ball.x, ball.y) < ball.radius THEN
                    dragging = true
                END IF
            END IF
        ELSE
            IF dist(mouseX, mouseY, ball.x, ball.y) < ball.radius THEN hovering = true ELSE hovering = false
            mouseDown = false
            IF dragging THEN
                dragging = false
                ball.velocity = 0
            END IF
        END IF

        IF NOT dragging THEN
            ball.acceleration = (-1 * g / ball.arm) * SIN(ball.angle)
            ball.velocity = ball.velocity + ball.acceleration
            'ball.velocity = ball.velocity * ball.damping
            ball.angle = ball.angle + ball.velocity
        ELSE
            diff.y = ball.origin.Y - mouseY
            diff.x = ball.origin.X - mouseX
            ball.angle = _ATAN2(-1 * diff.y, diff.x) - _D2R(90)
        END IF

        ball.x = ball.origin.X + (ball.arm * SIN(ball.angle))
        ball.y = ball.origin.Y + (ball.arm * COS(ball.angle))

        LINE (0, 0)-(_WIDTH, _HEIGHT), _RGBA32(0, 0, 0, 170), BF

        LOCATE 1, 1
        PRINT ball.damping
        PRINT ball.radius
        PRINT ball.velocity
        PRINT ball.acceleration
        PRINT ball.origin.X
        PRINT ball.origin.Y
        PRINT ball.angle
        PRINT ball.arm
        PRINT ball.x
        PRINT ball.y

        DIM c AS _UNSIGNED LONG
        LINE (ball.x, ball.y)-(ball.origin.X, ball.origin.Y), _RGB32(255, 255, 255)
        IF NOT hovering AND NOT dragging THEN c = _RGB32(255, 255, 255)
        IF hovering AND NOT dragging THEN c = _RGB32(0, 100, 100)
        IF dragging THEN c = _RGB32(0, 255, 255)
        CircleFill ball.x, ball.y, ball.radius, c
        LINE (0, ball.y)-STEP(10, 0), _RGB32(0, 255, 0)
        LINE (ball.x, _HEIGHT)-STEP(0, -10), _RGB32(0, 255, 0)
        _DISPLAY
        _LIMIT 60
    LOOP UNTIL _KEYHIT = 27
LOOP

FUNCTION dist! (x1!, y1!, x2!, y2!)
    dist! = SQR((x2! - x1!) ^ 2 + (y2! - y1!) ^ 2)
END FUNCTION

SUB CircleFill (CX AS LONG, CY AS LONG, R AS LONG, C AS _UNSIGNED LONG)
    'This sub from here: http://www.qb64.net/forum/index.php?topic=1848.msg17254#msg17254
    DIM Radius AS LONG
    DIM RadiusError AS LONG
    DIM X AS LONG
    DIM Y AS LONG

    Radius = ABS(R)
    RadiusError = -Radius
    X = Radius
    Y = 0

    IF Radius = 0 THEN PSET (CX, CY), C: EXIT SUB

    ' Draw the middle span here so we don't draw it twice in the main loop,
    ' which would be a problem with blending turned on.
    LINE (CX - X, CY)-(CX + X, CY), C, BF

    WHILE X > Y

        RadiusError = RadiusError + Y * 2 + 1

        IF RadiusError >= 0 THEN

            IF X <> Y + 1 THEN
                LINE (CX - Y, CY - X)-(CX + Y, CY - X), C, BF
                LINE (CX - Y, CY + X)-(CX + Y, CY + X), C, BF
            END IF

            X = X - 1
            RadiusError = RadiusError - X * 2

        END IF

        Y = Y + 1

        LINE (CX - X, CY - Y)-(CX + X, CY - Y), C, BF
        LINE (CX - X, CY + Y)-(CX + X, CY + Y), C, BF

    WEND

END SUB

