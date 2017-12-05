CONST true = -1, false = 0
CONST showVars = false

_TITLE "I came in like a wreeeeeecking ball..."

gameScreen = _NEWIMAGE(800, 600, 32)
SCREEN gameScreen
COLOR , _RGBA32(0, 0, 0, 0)
arena = _NEWIMAGE(4800, 600, 32)
arenaBG = _COPYIMAGE(arena)

drawArena

_DEST arena
camera = 0
g = .4
ball.impulse = 1.005 '.995
ball.radius = 30
ball.velocity = 0
ball.acceleration = 0
ball.origin.X = _WIDTH(gameScreen) / 2
ball.origin.Y = 0
ball.x = 0
ball.y = 0

level = 1
levelOSD = TIMER

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

        _DEST arena
        _PUTIMAGE , arenaBG

        DIM c AS _UNSIGNED LONG
        c = _RGB32(255, 0, 0)
        CircleFill ball.x, ball.y, ball.radius, _RGB32(255, 255, 255)
        CircleFill ball.x, ball.y, ball.radius - 2, c


        _DEST 0
        LINE (0, ball.y)-STEP(10, 0), _RGB32(0, 255, 0)
        LINE (ball.x, _HEIGHT)-STEP(0, -10), _RGB32(0, 255, 0)

        _PUTIMAGE (camera, 0), arena, _DISPLAY

        IF TIMER - levelOSD < 1.5 THEN
            m$ = "Level" + STR$(level)
            _PRINTSTRING (_WIDTH / 2 - _PRINTWIDTH(m$) / 2, _HEIGHT / 2 - _FONTHEIGHT / 2), m$
        END IF
        m$ = STR$(TIMER - levelOSD)
        m$ = LEFT$(m$, INSTR(m$, ".") + 1)
        _PRINTSTRING (_WIDTH - _PRINTWIDTH(m$), _HEIGHT - _FONTHEIGHT), m$

        _DISPLAY
        _LIMIT 60
    LOOP UNTIL _KEYHIT = 13

    ball.velocity = 0
    ball.acceleration = 0
    ball.origin.X = ball.x + _WIDTH(gameScreen) / 4
    IF ball.origin.X > _WIDTH(arena) THEN
        finished = true
        ball.origin.Y = 0
    ELSE
        DO
            ball.origin.Y = _RED32(POINT(ball.origin.X, 0))
            IF ball.origin.Y > 0 THEN EXIT DO
            ball.origin.X = ball.origin.X + 1
        LOOP
    END IF
    diff.y = ball.origin.Y - ball.y
    diff.x = ball.origin.X - ball.x
    ball.angle = _ATAN2(-1 * diff.y, diff.x) - _D2R(90)
    ball.arm = dist(ball.x, ball.y, ball.origin.X, ball.origin.Y)

    DO
        ball.acceleration = (-1 * g / ball.arm) * SIN(ball.angle)
        ball.velocity = ball.velocity + ball.acceleration
        ball.velocity = ball.velocity * ball.impulse
        ball.angle = ball.angle + ball.velocity

        ball.x = ball.origin.X + (ball.arm * SIN(ball.angle))
        ball.y = ball.origin.Y + (ball.arm * COS(ball.angle))

        _DEST arena
        _PUTIMAGE , arenaBG

        LINE (ball.x, ball.y)-(ball.origin.X, ball.origin.Y), _RGB32(255, 255, 255)
        CircleFill ball.x, ball.y, ball.radius, _RGB32(255, 255, 255)
        CircleFill ball.x, ball.y, ball.radius - 2, c

        _DEST 0

        IF ball.x + camera > _WIDTH / 2 + _WIDTH / 5 THEN
            camera = (_WIDTH / 2 + _WIDTH / 5) - ball.x
        ELSEIF ball.x + camera < _WIDTH / 3 THEN
            camera = _WIDTH / 3 - ball.x
        END IF

        IF camera > 0 THEN camera = 0
        IF camera < -(_WIDTH(arena) - _WIDTH(gameScreen)) THEN camera = -(_WIDTH(arena) - _WIDTH(gameScreen))

        _PUTIMAGE (camera, 0), arena, _DISPLAY

        IF showVars THEN
            LOCATE 1, 1
            PRINT ball.impulse
            PRINT ball.radius
            PRINT ball.velocity
            PRINT ball.acceleration
            PRINT ball.origin.X
            PRINT ball.origin.Y
            PRINT ball.angle
            PRINT ball.arm
            PRINT ball.x
            PRINT ball.y
            PRINT camera
        END IF

        IF finished THEN
            m$ = "You made it!"
            _PRINTSTRING (_WIDTH / 2 - _PRINTWIDTH(m$) / 2, _HEIGHT / 2 - _FONTHEIGHT / 2), m$
        ELSE
            IF TIMER - levelOSD < 1.5 THEN
                m$ = "Level" + STR$(level)
                _PRINTSTRING (_WIDTH / 2 - _PRINTWIDTH(m$) / 2, _HEIGHT / 2 - _FONTHEIGHT / 2), m$
            END IF

            m$ = STR$(TIMER - levelOSD)
            m$ = LEFT$(m$, INSTR(m$, ".") + 1)
            _PRINTSTRING (_WIDTH - _PRINTWIDTH(m$), _HEIGHT - _FONTHEIGHT), m$
        END IF

        _DISPLAY
        _LIMIT 60

        IF NOT finished THEN k = _KEYHIT ELSE k = 0
        IF ball.x - ball.radius > _WIDTH(arena) THEN madeIt = true
    LOOP UNTIL k = -13 OR madeIt

    IF finished OR madeIt THEN
        finished = false
        madeIt = false
        timeFinished = TIMER
        t.m$ = STR$(timeFinished - levelOSD)
        t.m$ = LEFT$(t.m$, INSTR(t.m$, ".") + 1)

        _PUTIMAGE (camera, 0), arenaBG, _DISPLAY

        m$ = "You made it in" + t.m$ + " seconds!"
        _PRINTSTRING (_WIDTH / 2 - _PRINTWIDTH(m$) / 2, _HEIGHT / 2 - _FONTHEIGHT / 2), m$
        m$ = "(hit space)"
        _PRINTSTRING (_WIDTH / 2 - _PRINTWIDTH(m$) / 2, _HEIGHT / 2 - _FONTHEIGHT / 2 + _FONTHEIGHT), m$

        DO UNTIL _KEYHIT = 32: _DISPLAY: _LIMIT 30: LOOP

        camera = 0
        ball.x = 0
        ball.y = 0
        ball.acceleration = 0
        ball.velocity = 0
        level = level + 1
        levelOSD = TIMER
        drawArena
    END IF

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


SUB drawArena
    SHARED arenaBG
    _DEST arenaBG
    _BLEND
    CLS , _RGB32(255, 255, 255)
    FOR i = 1 TO 200
        LINE (RND * _WIDTH, RND * _HEIGHT)-(RND * _WIDTH, RND * _HEIGHT), _RGBA32(RND * 256, RND * 256, RND * 256, RND * 200), BF
    NEXT

    blockSize = 100
    margin = 3
    FOR i = 0 TO _WIDTH STEP blockSize
        h = RND * 150 + 50
        LINE (i, 0)-STEP(100, h), _RGB32(0, 0, 0), BF
        LINE (i + margin, 0)-STEP(blockSize - (margin * 2), h - margin), _RGB32(h, h, h), BF

        h = RND * 100 + 50
        LINE (i, _HEIGHT)-STEP(100, -h), _RGB32(0, 0, 0), BF
        LINE (i + margin, _HEIGHT)-STEP(blockSize - (margin * 2), -(h - margin)), _RGB32(h, h, h), BF
    NEXT
    _DONTBLEND
    _SOURCE arenaBG
END SUB

