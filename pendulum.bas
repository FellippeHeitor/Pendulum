'Fireball whoosh sound: https://freesound.org/s/248116/
OPTION _EXPLICIT
CONST true = -1, false = 0

_TITLE "I came in like a wreeeeeecking ball..."

DIM SHARED gameWidth AS INTEGER, gameHeight AS INTEGER
DIM SHARED arenaWidth AS INTEGER, arenaHeight AS INTEGER
DIM SHARED bgWidth AS INTEGER, bgHeight AS INTEGER
DIM SHARED gameScreen AS LONG ', arenaBG AS LONG
DIM SHARED level AS LONG, camera AS SINGLE
DIM SHARED blockOffset AS INTEGER, respawnOffset AS SINGLE
DIM SHARED ball.impulse, ball.radius, ball.y.velocity
DIM SHARED ball.y.acceleration, ball.x.velocity, ball.x.acceleration
DIM SHARED ball.origin.x, ball.origin.y, ball.x, ball.y
DIM SHARED level.g, level.r, g AS SINGLE, k AS LONG
DIM SHARED started AS _BYTE, p AS LONG, tx, ty, madeIt AS _BYTE
DIM SHARED cameraCenter AS SINGLE, cameraCenterY AS SINGLE, cameraY AS SINGLE
DIM SHARED timerSet AS _BYTE, m$, levelStarted AS SINGLE, finished AS _BYTE
DIM SHARED diff.y, diff.x, ball.angle, ball.arm, timeFinished AS SINGLE
DIM SHARED t.m$, mag, showBG AS _BYTE, blockLoopStart AS LONG
DIM SHARED ballHit AS _BYTE, glowRadius AS INTEGER
DIM SHARED ellipsisPlot AS INTEGER, waitForRelease AS _BYTE, willRespawn AS _BYTE
DIM SHARED gravitationalFloat AS _BYTE, g.i AS _BYTE
DIM SHARED bigPortal AS LONG, smallPortal AS LONG
DIM SHARED frameRate AS INTEGER, frameRateRestoreTimer AS SINGLE
DIM SHARED respawnDelay AS SINGLE, hideRed AS SINGLE
DIM SHARED largeFont AS LONG, smallFont AS LONG
DIM SHARED prev.m$
DIM SHARED whooshSound AS LONG

CONST maxGravitationalFloat = 10
CONST maxGlowRadius = 25

gameWidth = 900
gameHeight = 650
cameraCenter = 3
cameraCenterY = 2
frameRate = 60
respawnDelay = 2

gameScreen = _NEWIMAGE(gameWidth, gameHeight, 32)
SCREEN gameScreen
COLOR , _RGBA32(0, 0, 0, 0)
arenaWidth = gameWidth * 15
arenaHeight = gameHeight * 1.5
bgWidth = gameWidth * 30
bgHeight = gameHeight * 1.5
showBG = true

'largeFont = _LOADFONT("cour.ttf", 40, "monospace")
IF largeFont = 0 THEN largeFont = 16
'smallFont = _LOADFONT("cour.ttf", 20, "monospace")
IF smallFont = 0 THEN smallFont = 16

whooshSound = _SNDOPEN("assets/whoosh.wav")

CONST FIRE = 1
CONST SMOKE = 2
CONST SPARK = 3
CONST SOOTSTAIN = 4
CONST GRAVITATOR = 5
CONST GRAVITATORCELL = 6
CONST BUSTEDCELL = 7

TYPE newItem
    x AS SINGLE
    y AS SINGLE
    w AS SINGLE
    h AS SINGLE
    yAcc AS SINGLE
    yVel AS SINGLE
    xAcc AS SINGLE
    xVel AS SINGLE
    a AS SINGLE
    b AS SINGLE
    minA AS SINGLE
    maxA AS SINGLE
    minb AS SINGLE
    maxB AS SINGLE
    multA AS _BYTE
    multB AS _BYTE
    kind AS INTEGER
    color AS _UNSIGNED LONG
    red AS _UNSIGNED _BYTE
    green AS _UNSIGNED _BYTE
    blue AS _UNSIGNED _BYTE
    active AS _BYTE
    parent AS LONG
    generation AS INTEGER
    maxGeneration AS INTEGER
END TYPE

CONST maxBGDecoration = 50
REDIM SHARED bg(1 TO maxBGDecoration) AS newItem
REDIM SHARED block(200) AS newItem
REDIM SHARED particle(0) AS newItem
REDIM SHARED deathNote(1 TO 1) AS STRING, deathNoteIndex AS INTEGER
DIM SHARED totalBlocks AS LONG
DIM thisParticle AS LONG

CONST maxRND = 1000000
DIM SHARED rndTable(1 TO maxRND) AS SINGLE
DIM SHARED rndSeed AS LONG, rndIndex AS LONG

RANDOMIZE 3
DIM i&
FOR i& = 1 TO maxRND
    rndTable(i&) = RND
NEXT

i& = 0
RESTORE deathNotes
DO
    READ m$
    IF m$ = "-" THEN EXIT DO
    i& = i& + 1
    IF i& > UBOUND(deathNote) THEN REDIM _PRESERVE deathNote(1 TO i&) AS STRING
    deathNote(i&) = m$
LOOP
IF i& = 0 THEN deathNote(1) = "OUCH!"

deathNotes:
DATA "OUCH!","THAT MUST HURT...","WASTED","CAN'T TOUCH THIS!","BLOCKS ARE LAVA.","-"

camera = 0
ellipsisPlot = 63
blockOffset = 100
respawnOffset = 2
ball.impulse = 1.003 '.damping = .995
ball.radius = 20
ball.y.velocity = 0
ball.y.acceleration = 0
ball.x.velocity = 0
ball.x.acceleration = 0
ball.origin.x = _WIDTH(gameScreen) / 2
ball.origin.y = 0
ball.x = ball.radius * 1.5
ball.y = arenaHeight / respawnOffset
deathNoteIndex = _CEIL(getRND * UBOUND(deathNote))

DIM SHARED state AS _BYTE
CONST HALTED = 0
CONST FALLING = 1
CONST SWINGING = 2

level = 1
setRand level
generateArena

level.g = getRND * 256
level.r = getRND * 256

DO
    IF _KEYDOWN(13) THEN
        IF NOT waitForRelease AND NOT willRespawn THEN
            IF state <> SWINGING THEN
                IF whooshSound > 0 THEN _SNDPLAYCOPY whooshSound
                state = SWINGING
                started = true
                IF timerSet = false THEN
                    timerSet = true
                    levelStarted = TIMER
                END IF

                ball.y.acceleration = ball.y.acceleration / 25
                ball.y.velocity = ball.y.acceleration
                ball.origin.x = ball.x + _WIDTH(gameScreen) / _CEIL(map(ball.y, 0, _HEIGHT, 6, 4))
                IF ball.origin.x > arenaWidth THEN
                    finished = true
                    ball.origin.y = 0
                ELSE
                    'DO
                    ball.origin.y = _BLUE32(POINT(ball.origin.x + camera, 0))
                    '    IF ball.origin.y > 0 THEN EXIT DO
                    '    ball.origin.x = ball.origin.x + 1
                    'LOOP
                END IF
                ball.origin.y = ball.origin.y + blockOffset
                diff.y = ball.origin.y - ball.y
                diff.x = ball.origin.x - ball.x
                ball.angle = _ATAN2(-1 * diff.y, diff.x) - _D2R(90)
                ball.arm = dist(ball.x, ball.y, ball.origin.x, ball.origin.y)
            END IF
        END IF
    ELSE
        IF waitForRelease THEN
            waitForRelease = false
        ELSE
            IF state = SWINGING AND NOT finished THEN
                state = FALLING
                mag = ball.y.velocity * 1000
                IF mag > 10 THEN mag = 10
                IF mag < -10 THEN mag = -10
                ball.x.velocity = COS(ball.angle) * mag
                ball.y.velocity = -SIN(ball.angle) * mag
            END IF
        END IF
    END IF

    k = _KEYHIT
    IF k = 9 AND _KEYDOWN(100304) = false THEN GOTO setNewLevel
    IF k = 9 AND _KEYDOWN(100304) AND level > 1 THEN level = level - 2: GOTO setNewLevel
    IF k = ASC("B") OR k = ASC("b") THEN showBG = NOT showBG
    IF k = ASC("W") OR k = ASC("w") THEN glowRadius = glowRadius + 1
    IF k = ASC("S") OR k = ASC("s") THEN glowRadius = glowRadius + (glowRadius > 1)

    doPhysics

    IF ball.x - ball.radius > arenaWidth THEN madeIt = true

    doCamera

    IF willRespawn THEN LINE (0, 0)-STEP(_WIDTH, _HEIGHT), _RGBA32(0, 0, 0, 30), BF ELSE CLS
    IF showBG THEN
        drawBG
    END IF
    drawBlocks

    IF state = SWINGING THEN
        ThickLine ball.x + camera, ball.y + cameraY, ball.origin.x + camera, ball.origin.y + cameraY, _RGB32(0, 0, 0), 8
        ThickLine ball.x + camera, ball.y + cameraY, ball.origin.x + camera, ball.origin.y + cameraY, _RGB32(255, 255, 255), 4
    END IF

    IF NOT willRespawn THEN
        FOR p = 1 TO 30
            tx = ball.x + COS(p) * (getRND * ball.radius)
            ty = ball.y + SIN(p) * (getRND * ball.radius)
            thisParticle = addParticle(tx, ty, FIRE, 0)
        NEXT
        FOR p = ball.radius + glowRadius TO ball.radius + 1 STEP -1
            CircleFill ball.x + camera, ball.y + cameraY, p, _RGBA32(238, 216, 94, map(p, ball.radius + 1, ball.radius + glowRadius, 15, 5))
        NEXT
        CircleFill ball.x + camera, ball.y + cameraY, ball.radius, _RGBA32(255, 255, 255, 200)
    END IF

    doParticles

    IF state = HALTED THEN
        IF NOT timerSet THEN m$ = "Hold ENTER to start..." ELSE m$ = "Hold ENTER to continue..."
        IF willRespawn THEN
            m$ = deathNote(deathNoteIndex)
            LINE (0, 0)-(_WIDTH, _HEIGHT), _RGBA32(72, 0, 0, 180), BF
        ELSE
            IF TIMER - hideRed < respawnDelay / 2 THEN
                LINE (0, 0)-(_WIDTH, _HEIGHT), _RGBA32(72, 0, 0, map(TIMER - hideRed, 0, respawnDelay / 2, 180, 0)), BF
            END IF
        END IF
        _FONT largeFont
        _PRINTSTRING (_WIDTH / 2 - _PRINTWIDTH(m$) / 2, _HEIGHT / 2 - _FONTHEIGHT / 2), m$
    END IF

    IF timerSet THEN
        m$ = STR$(TIMER - levelStarted)
        m$ = LEFT$(m$, INSTR(m$, ".") + 1)
        _FONT smallFont
        _PRINTSTRING (_WIDTH - _PRINTWIDTH(m$), _HEIGHT - _FONTHEIGHT), m$
    END IF

    IF finished THEN
        m$ = "You made it!"
        _FONT largeFont
        _PRINTSTRING (_WIDTH / 2 - _PRINTWIDTH(m$) / 2, _HEIGHT / 2 - _FONTHEIGHT / 2), m$
    ELSE
        IF started THEN
            m$ = STR$(TIMER - levelStarted)
            m$ = LEFT$(m$, INSTR(m$, ".") + 1)
            _FONT smallFont
            _PRINTSTRING (_WIDTH - _PRINTWIDTH(m$), _HEIGHT - _FONTHEIGHT), m$
        END IF
    END IF

    _FONT smallFont
    m$ = "Small portals:" + STR$(smallPortal)
    _PRINTSTRING (0, 0), m$
    m$ = "Big portals:" + STR$(bigPortal)
    _PRINTSTRING (0, _FONTHEIGHT), m$

    _DISPLAY
    IF frameRate < 60 AND TIMER - frameRateRestoreTimer > respawnDelay THEN
        frameRate = 60
        willRespawn = false
        hideRed = TIMER
    END IF
    _LIMIT frameRate

    IF ball.x - ball.radius > arenaWidth THEN madeIt = true

    IF madeIt THEN
        timeFinished = TIMER
        t.m$ = STR$(timeFinished - levelStarted)
        t.m$ = LEFT$(t.m$, INSTR(t.m$, ".") + 1)
        IF _KEYDOWN(13) THEN waitForRelease = true
        _FONT largeFont
        DO
            IF showBG THEN
                IF willRespawn THEN LINE (0, 0)-STEP(_WIDTH, _HEIGHT), _RGBA32(0, 0, 0, 30), BF
                drawBG
            ELSE
                IF willRespawn THEN LINE (0, 0)-STEP(_WIDTH, _HEIGHT), _RGBA32(0, 0, 0, 30), BF ELSE CLS
            END IF
            drawBlocks
            doParticles

            m$ = "You made it in" + t.m$ + " seconds!"
            _PRINTSTRING (_WIDTH / 2 - _PRINTWIDTH(m$) / 2, _HEIGHT / 2 - _FONTHEIGHT / 2), m$
            m$ = "(hit ENTER)"
            _PRINTSTRING (_WIDTH / 2 - _PRINTWIDTH(m$) / 2, _HEIGHT / 2 - _FONTHEIGHT / 2 + _FONTHEIGHT), m$

            IF _KEYDOWN(13) AND NOT waitForRelease THEN
                EXIT DO
            ELSEIF NOT _KEYDOWN(13) AND waitForRelease THEN
                waitForRelease = false
            END IF

            _DISPLAY
            _LIMIT 60
        LOOP

        setNewLevel:
        FOR p = 1 TO UBOUND(particle)
            particle(p).active = false
        NEXT
        finished = false
        madeIt = false
        glowRadius = 0
        camera = 0
        cameraY = 0
        ball.x = ball.radius
        ball.y = arenaHeight / respawnOffset
        ball.y.acceleration = 0
        ball.y.velocity = 0
        ball.x.acceleration = 0
        ball.x.velocity = 0
        level = level + 1
        setRand level
        generateArena
        started = false
        state = HALTED
        timerSet = false
        level.g = getRND * 256
        level.r = getRND * 256
    END IF
LOOP

SUB doPhysics
    IF state = FALLING THEN
        g = .1
        ball.y.acceleration = ball.y.acceleration + g / 10
        ball.y.velocity = ball.y.velocity + ball.y.acceleration
        ball.y = ball.y + ball.y.velocity

        ball.x = ball.x + ball.x.velocity
    ELSEIF state = SWINGING THEN
        g = .9
        ball.y.acceleration = (-1 * g / ball.arm) * SIN(ball.angle)
        ball.y.velocity = ball.y.velocity + ball.y.acceleration
        ball.y.velocity = ball.y.velocity * ball.impulse

        ball.angle = ball.angle + ball.y.velocity

        ball.x = ball.origin.x + (ball.arm * SIN(ball.angle))
        ball.y = ball.origin.y + (ball.arm * COS(ball.angle))
    END IF

    IF ball.y - ball.radius / 2 > arenaHeight OR ballHit THEN
        state = HALTED
        deathNoteIndex = _CEIL(getRND * UBOUND(deathNote))
        waitForRelease = true
        willRespawn = true
        slowMo
        glowRadius = 0
        ball.y = arenaHeight / respawnOffset
        ball.y.acceleration = 0
        ball.y.velocity = 0
        ball.x.acceleration = 0
        ball.x.velocity = 0
    END IF
END SUB

FUNCTION checkCollision%% (x1!, y1!, r!, x!, y!, w!, h!)
    '//solution from https://jsfiddle.net/TappT/z5r21nu0/16/
    'function collisionNormal(_ball, _rect) {
    '  if(_ball.pos.x + _ball.r > _rect.pos.x &&
    '     _ball.pos.x - _ball.r < (_rect.pos.x + _rect.w) &&
    '     _ball.pos.y + _ball.r > _rect.pos.y &&
    '     _ball.pos.y - _ball.r < (_rect.pos.y + _rect.h))
    '  {
    checkCollision%% = (x1! + camera) + r! > x! AND _
                       (x1! + camera) - r! < (x! + w!) AND _
                       (y1! + cameraY) + r! > y! AND _
                       (y1! + cameraY) - r! < (y! + h!)
END FUNCTION

SUB doCamera
    DIM new.cameraY AS SINGLE

    IF ball.x + camera > _WIDTH / cameraCenter THEN
        camera = (_WIDTH / cameraCenter) - ball.x
    ELSEIF ball.x + camera < _WIDTH / cameraCenter THEN
        camera = _WIDTH / cameraCenter - ball.x
    END IF

    IF ball.y + cameraY > _HEIGHT / cameraCenterY THEN
        new.cameraY = (_HEIGHT / cameraCenterY) - ball.y
    ELSEIF ball.y + cameraY < _HEIGHT / cameraCenterY THEN
        new.cameraY = _HEIGHT / cameraCenterY - ball.y
    END IF

    IF state = HALTED THEN
        DIM a AS SINGLE
        a = ABS(cameraY - new.cameraY)
        IF a > 3 THEN a = 3
        IF a < 3 THEN a = 0
        IF cameraY > new.cameraY THEN
            cameraY = cameraY - a
        ELSEIF cameraY < new.cameraY THEN
            cameraY = cameraY + a
        ELSE
            cameraY = new.cameraY
        END IF
    ELSE
        cameraY = new.cameraY
    END IF

    IF camera > 0 THEN camera = 0
    IF camera < -(arenaWidth - _WIDTH(gameScreen)) THEN camera = -(arenaWidth - _WIDTH(gameScreen))

    IF cameraY > 0 THEN cameraY = 0
    IF cameraY < -(arenaHeight - _HEIGHT(gameScreen)) THEN cameraY = -(arenaHeight - _HEIGHT(gameScreen))
END SUB

FUNCTION dist! (x1!, y1!, x2!, y2!)
    dist! = _HYPOT((x2! - x1!), (y2! - y1!))
END FUNCTION

FUNCTION map! (value!, minRange!, maxRange!, newMinRange!, newMaxRange!)
    map! = ((value! - minRange!) / (maxRange! - minRange!)) * (newMaxRange! - newMinRange!) + newMinRange!
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


SUB generateArena
    DIM i AS LONG, blockSize AS INTEGER, lastGravitator AS LONG
    DIM w AS SINGLE, h AS SINGLE, x AS SINGLE, y AS SINGLE, s1 AS SINGLE, s2 AS SINGLE
    DIM thisParticle AS LONG, minimumDistance AS INTEGER

    _FONT largeFont
    m$ = "Level" + STR$(level)

    CLS , _RGB32(255, 255, 255)
    FOR i = 1 TO maxBGDecoration
        bg(i).x = getRND * (bgWidth * .6) - 200
        bg(i).y = getRND * (bgHeight * .6) - 200
        bg(i).w = getRND * bgWidth
        bg(i).h = getRND * bgHeight
        bg(i).color = _RGB32(50 + getRND * 100, 50 + getRND * 100, getRND * 50)
        LINE (bg(i).x / 30, bg(i).y / 1.5)-STEP(bg(i).w / 30, bg(i).h / 1.5), bg(i).color, BF
        COLOR _RGB32(0, 0, 0), 0
        _PRINTSTRING (_WIDTH / 2 - _PRINTWIDTH(m$) / 2 + 1, _HEIGHT / 2 - _FONTHEIGHT / 2 + 1), m$
        COLOR _RGB32(255, 255, 255), 0
        _PRINTSTRING (_WIDTH / 2 - _PRINTWIDTH(m$) / 2, _HEIGHT / 2 - _FONTHEIGHT / 2), m$
        _DISPLAY
        _LIMIT 25
    NEXT

    '_DEST arenaBG
    'FOR i = 0 TO _WIDTH STEP blockSize
    '    LINE (i, 0)-STEP(blockSize - 1, _HEIGHT), _RGBA32(0, 0, 0, map(i, 0, _WIDTH, 190, 80)), BF
    'NEXT

    blockSize = 100
    totalBlocks = 0
    minimumDistance = 5

    FOR i = 0 TO arenaWidth STEP blockSize
        'top block
        h = getRND * 256 '206 + 50
        s1 = h + blockOffset
        y = 0
        GOSUB addBlock

        'bottom block
        h = getRND * 256 '206 + 50
        y = arenaHeight - h - blockOffset
        s2 = y
        GOSUB addBlock

        IF (s2 - s1) > 200 AND i - lastGravitator > blockSize * minimumDistance AND i < arenaWidth - blockSize * 2 THEN 'AND (s2 - s1) < 450
            'add gravitators between these blocks
            thisParticle = addParticle(i + blockSize / 2, s1 + _CEIL(getRND * (s2 - s1)), GRAVITATOR, 0)
            lastGravitator = i + blockSize / 2
            particle(thisParticle).a = 25
            particle(thisParticle).minA = 1
            particle(thisParticle).maxA = 100
            particle(thisParticle).multA = -1
            particle(thisParticle).b = 150
            particle(thisParticle).minb = 150
            particle(thisParticle).maxB = 150
            particle(thisParticle).multB = 1

            minimumDistance = 5 + INT(getRND * 10)
        END IF
    NEXT
    EXIT SUB

    addBlock:
    totalBlocks = totalBlocks + 1
    IF totalBlocks > UBOUND(block) THEN REDIM _PRESERVE block(UBOUND(block) + 99) AS newItem
    block(totalBlocks).x = i
    block(totalBlocks).y = y
    block(totalBlocks).h = h
    block(totalBlocks).w = blockSize
    RETURN

END SUB

SUB ThickLine (x, y, x1, y1, c AS _UNSIGNED LONG, t)
    DIM i AS SINGLE, xx AS SINGLE, yy AS SINGLE
    'This sub from http://www.qb64.net/forum/index.php?topic=1456.msg11548#msg11548
    FOR i = 0 TO 1 STEP .01
        xx = INT((x1 - x) * i) + x
        yy = INT((y1 - y) * i) + y
        CircleFill xx, yy, t \ 2, c
    NEXT
END SUB

SUB drawBlocks
    DIM margin, j AS LONG
    DIM i AS SINGLE, y AS SINGLE, h AS SINGLE
    DIM blockSize AS SINGLE

    margin = 3
    ballHit = false
    blockLoopStart = INT(ABS(camera) / (block(1).w / 2)) - 2
    IF blockLoopStart < 1 THEN blockLoopStart = 1
    FOR j = blockLoopStart TO totalBlocks
        i = block(j).x + camera
        IF i > _WIDTH(0) THEN EXIT FOR
        y = block(j).y + cameraY
        h = block(j).h
        blockSize = block(j).w
        LINE (i, y - margin)-STEP(blockSize, blockOffset + h + margin), _RGB32(0, 0, 0), BF
        LINE (i + margin, y)-STEP(blockSize - (margin * 2), blockOffset + h - margin), _RGB32(level.r, level.g, h), BF
        IF ballHit = false THEN
            ballHit = checkCollision(ball.x, ball.y, ball.radius, i, y - margin, blockSize, blockOffset + h + margin)
        END IF
    NEXT
END SUB

FUNCTION addParticle (x AS SINGLE, y AS SINGLE, kind AS INTEGER, parent AS LONG)
    DIM i AS LONG
    DIM newParticle AS LONG, thisParticle AS LONG
    DIM a AS SINGLE
    DIM s AS SINGLE
    STATIC previousParent AS LONG, childrenCount AS LONG

    FOR i = 1 TO UBOUND(particle)
        IF NOT particle(i).active THEN newParticle = i: EXIT FOR
    NEXT

    IF newParticle = 0 THEN
        newParticle = UBOUND(particle) + 1
        REDIM _PRESERVE particle(1 TO UBOUND(particle) + 99) AS newItem
    END IF

    particle(newParticle).x = x
    particle(newParticle).xVel = 0
    particle(newParticle).xAcc = 0
    particle(newParticle).y = y
    particle(newParticle).yVel = 0
    particle(newParticle).yAcc = 0
    particle(newParticle).kind = kind
    particle(newParticle).parent = parent
    particle(newParticle).generation = 0
    particle(newParticle).active = true

    a = getRND * _PI(2)

    SELECT CASE kind
        CASE FIRE
            particle(newParticle).yVel = SIN(a) * 2
            particle(newParticle).xVel = COS(a) * 2
        CASE SPARK
            particle(newParticle).yVel = SIN(a) * 3
            particle(newParticle).xVel = COS(a) * 3
            particle(newParticle).maxGeneration = 25
        CASE SOOTSTAIN
            particle(newParticle).maxGeneration = 200
        CASE SMOKE
            particle(newParticle).maxGeneration = 15
        CASE GRAVITATOR
            FOR a = 1 TO ellipsisPlot * 2
                thisParticle = addParticle(x + COS(a) * particle(newParticle).a, y + SIN(a) * particle(newParticle).b, GRAVITATORCELL, newParticle)
            NEXT
        CASE GRAVITATORCELL
            IF parent <> previousParent THEN
                previousParent = parent
                childrenCount = 0
            END IF
            childrenCount = childrenCount + 1
            IF childrenCount < ellipsisPlot THEN
                particle(newParticle).red = 233
                particle(newParticle).green = 211
                particle(newParticle).blue = 72
            ELSE
                particle(newParticle).red = 105
                particle(newParticle).green = 216
                particle(newParticle).blue = 94
            END IF
    END SELECT

    addParticle = newParticle
END FUNCTION

SUB doParticles
    STATIC frames AS _INTEGER64
    DIM i AS LONG, j AS LONG, k AS SINGLE
    DIM gravity AS SINGLE
    DIM g AS _UNSIGNED _BYTE, b AS _UNSIGNED _BYTE, a AS _UNSIGNED _BYTE
    DIM s AS SINGLE, thisParticle AS LONG

    gravity = .02

    frames = frames + 1

    IF g.i = 0 THEN g.i = 1

    IF frames MOD 10 = 0 THEN
        gravitationalFloat = gravitationalFloat + g.i
        IF gravitationalFloat > maxGravitationalFloat THEN g.i = g.i * -1
        IF gravitationalFloat < -maxGravitationalFloat THEN g.i = g.i * -1
    END IF

    FOR i = 1 TO UBOUND(particle)
        IF particle(i).active THEN
            particle(i).generation = particle(i).generation + 1

            particle(i).yVel = particle(i).yVel + particle(i).yAcc
            particle(i).y = particle(i).y + particle(i).yVel

            particle(i).xVel = particle(i).xVel + particle(i).xAcc
            particle(i).x = particle(i).x + particle(i).xVel

            SELECT EVERYCASE particle(i).kind
                CASE FIRE
                    'process
                    IF particle(i).generation > 8 THEN
                        particle(i).kind = SMOKE
                        particle(i).maxGeneration = 15
                        particle(i).generation = 0
                        particle(i).yVel = 0
                        particle(i).xVel = 0
                        particle(i).yAcc = -.4
                        particle(i).xAcc = -.2
                    ELSE
                        SELECT EVERYCASE particle(i).generation
                            CASE 1, 2: g = 238: b = 177: a = 200
                            CASE 3
                                s = getRND
                                IF s < .02 THEN thisParticle = addParticle(particle(i).x, particle(i).y, SPARK, i)
                            CASE 3, 4: g = 222: b = 89: a = 180
                            CASE 5, 6: g = 128: b = 50: a = 150
                            CASE 7, 8: g = 33: b = 0: a = 70
                        END SELECT
                        particle(i).color = _RGBA32(255, g, b, a)
                    END IF
                CASE SMOKE
                    'process
                    particle(i).color = _RGBA32(33, 17, 39, map(particle(i).generation, 1, particle(i).maxGeneration, 100, 0))
                    IF particle(i).generation > particle(i).maxGeneration THEN particle(i).active = false
                    'CASE SOOTSTAIN
                    '    particle(i).color = _RGBA32(33, 17, 39, map(particle(i).generation, 1, particle(i).maxGeneration, 40, 0))
                    '    IF particle(i).generation > particle(i).maxGeneration THEN particle(i).active = false
                CASE SPARK
                    'process
                    particle(i).yAcc = particle(i).yAcc + gravity
                    SELECT CASE particle(i).generation
                        CASE 1, 2: g = 238: b = 177
                        CASE 3, 4: g = 222: b = 89
                        CASE 5, 6: g = 128: b = 50
                        CASE 7, 8: g = 33: b = 0
                    END SELECT
                    a = map(particle(i).generation, 1, 15, 255, 0)
                    particle(i).color = _RGBA32(255, g, b, a)
                    IF particle(i).generation > particle(i).maxGeneration THEN particle(i).active = false

                    'show
                    FOR p = glowRadius TO 1 STEP -1
                        CircleFill particle(i).x + camera, particle(i).y + cameraY, p, _RGBA32(238, 216, 94, map(p, 1, glowRadius, 15, 5))
                    NEXT
                    CircleFill particle(i).x + camera, particle(i).y + cameraY, 1, particle(i).color
                CASE GRAVITATOR
                    'particle(i).a = particle(i).a + particle(i).multA
                    'IF particle(i).a > particle(i).maxA OR particle(i).a < particle(i).minA THEN
                    '    particle(i).multA = particle(i).multA * -1
                    '    particle(i).a = particle(i).a + particle(i).multA
                    'END IF
                    'particle(i).b = particle(i).b + particle(i).multB
                    'IF particle(i).b > particle(i).maxB OR particle(i).b < particle(i).minb THEN
                    '    particle(i).multB = particle(i).multB * -1
                    '    particle(i).b = particle(i).b + particle(i).multB
                    'END IF

                    k = 0
                    FOR j = i + 1 TO i + ellipsisPlot + 1
                        IF particle(j).parent = i THEN
                            k = k + .1
                            particle(j).x = particle(i).x + COS(k) * map(particle(j).x + camera, 0, _WIDTH(0), particle(i).minA, particle(i).maxA)
                            particle(j).y = particle(i).y + SIN(k) * particle(i).b
                        END IF
                    NEXT

                    k = 0
                    FOR j = i + ellipsisPlot + 1 TO i + ellipsisPlot * 2
                        IF particle(j).parent = i THEN
                            k = k + .1
                            particle(j).x = particle(i).x + COS(k) * map(particle(j).x + camera, 0, _WIDTH(0), particle(i).minA, particle(i).maxA / 3)
                            particle(j).y = particle(i).y + SIN(k) * particle(i).b / 4
                        END IF
                    NEXT

                    'ball VS gravitator collision detection:
                    IF dist(ball.x, ball.y, particle(i).x, particle(i).y + gravitationalFloat) < ball.radius * 2.5 THEN
                        smallPortal = smallPortal + 1
                        IF glowRadius < maxGlowRadius THEN glowRadius = glowRadius + 5
                        particle(i).active = false
                        FOR j = i + 1 TO i + ellipsisPlot * 2
                            IF particle(j).parent = i THEN
                                particle(j).kind = BUSTEDCELL
                                particle(j).generation = 0
                                particle(j).maxGeneration = 100
                                particle(j).yVel = SIN(getRND * _PI(2)) * 10
                                particle(j).xVel = COS(getRND * _PI(2)) * 10
                                particle(j).parent = -1
                            END IF
                        NEXT
                    ELSEIF ball.x > particle(i).x - ball.radius / 2 AND _
                           ball.x < particle(i).x + ball.radius / 2 AND _
                           ball.y > particle(i).y - 180 AND _
                           ball.y < particle(i).y + 180 THEN
                        bigPortal = bigPortal + 1
                        'glowRadius = 0
                        particle(i).active = false
                        FOR j = i + 1 TO i + ellipsisPlot * 2
                            IF particle(j).parent = i THEN
                                particle(j).kind = BUSTEDCELL
                                particle(j).generation = 0
                                particle(j).maxGeneration = 50
                                'particle(j).yVel = SIN(getRND * _PI(2)) * 10
                                particle(j).xVel = COS(getRND * _PI(2)) '* 2
                                particle(j).parent = -2
                            END IF
                        NEXT
                    END IF
                CASE BUSTEDCELL
                    particle(i).yAcc = particle(i).yAcc + gravity
                    IF particle(i).generation > particle(i).maxGeneration THEN
                        particle(i).active = false
                    ELSE
                        FOR j = 6 TO 4 STEP -1
                            CircleFill particle(i).x + camera, particle(i).y + cameraY + gravitationalFloat, 6, _RGBA32(particle(i).red, particle(i).green, particle(i).blue, map(j, 4, 6, 10, 25))
                        NEXT
                        IF particle(i).parent = -1 THEN
                            CircleFill particle(i).x + camera, particle(i).y + cameraY + gravitationalFloat, 3, _RGB32(255, 244, 238)
                        ELSEIF particle(i).parent = -2 THEN
                            CircleFill particle(i).x + camera, particle(i).y + cameraY + gravitationalFloat, 3, _RGB32(160, 160, 160)
                        END IF
                    END IF
                CASE FIRE, SMOKE
                    'show
                    s = dist(particle(i).x, particle(i).y, ball.x, ball.y)
                    s = map(s, 0, ball.radius * 4, 9, 2)
                    IF particle(i).parent > 0 THEN s = 4 'former gravitator cell
                    IF s < 2 THEN s = 2
                    CircleFill particle(i).x + camera, particle(i).y + cameraY, s, particle(i).color

                    'CASE SMOKE
                    '    IF state = HALTED THEN
                    '        DIM l AS LONG, j AS LONG, y AS SINGLE, h AS SINGLE, blockSize AS SINGLE, margin AS SINGLE
                    '        margin = 3
                    '        FOR j = blockLoopStart TO totalBlocks
                    '            l = block(j).x + camera
                    '            IF l > _WIDTH(0) THEN EXIT FOR
                    '            y = block(j).y + cameraY
                    '            h = block(j).h
                    '            blockSize = block(j).w
                    '            IF checkCollision(particle(i).x, particle(i).y, s, l, y - margin, blockSize, blockOffset + h + margin) THEN
                    '                addParticle particle(i).x, particle(i).y, SOOTSTAIN
                    '                EXIT FOR
                    '            END IF
                    '        NEXT
                    '    END IF

                    'CASE SOOTSTAIN
                    '    CircleFill particle(i).x + camera, particle(i).y + cameraY, 2, particle(i).color
                CASE GRAVITATORCELL
                    'show
                    FOR j = 6 TO 4 STEP -1
                        CircleFill particle(i).x + camera, particle(i).y + cameraY + gravitationalFloat, 6, _RGBA32(particle(i).red, particle(i).green, particle(i).blue, map(j, 4, 6, 10, 25))
                    NEXT
                    CircleFill particle(i).x + camera, particle(i).y + cameraY + gravitationalFloat, 3, _RGB32(255, 244, 238)
            END SELECT
        END IF
    NEXT
END SUB


SUB setRand (seed&)
    IF seed& > UBOUND(rndtable) OR seed& < 1 THEN
        rndIndex = 1
    ELSE
        rndIndex = seed&
    END IF
END SUB

FUNCTION getRND
    rndIndex = rndIndex + 1
    IF rndIndex > UBOUND(rndtable) THEN rndIndex = 1
    getRND = rndTable(rndIndex)
END FUNCTION

SUB slowMo
    frameRate = 20: frameRateRestoreTimer = TIMER
END SUB

SUB drawBG
    DIM i AS INTEGER
    FOR i = maxBGDecoration TO 1 STEP -1
        LINE (bg(i).x + camera / 2, bg(i).y + cameraY)-STEP(bg(i).w, bg(i).h), bg(i).color, BF
    NEXT
END SUB
