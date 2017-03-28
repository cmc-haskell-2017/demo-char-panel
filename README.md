# demo-char-panel

Экран выбора персонажа для ролевой игры.

![Экран выбора персонажа.](images/demo-char-panel.gif)

## Сборка и запуск

Клонируйте репозиторий:

```
git clone https://github.com/cmc-haskell-2017/demo-flappy-lambda.git
cd demo-flappy-lambda
```

Соберите проект при помощи [утилиты Stack](https://www.haskellstack.org):

```
stack setup
stack build
```

Собрать проект и запустить игру можно при помощи команды

```
stack build && stack exec demo-flappy-lambda
```

## Задание

В качестве задания к [лекции «Аппликативные функторы»](https://youtu.be/ckp60IxNH9s) требуется добавить недостающие поля в
[панель настроек персонажа](),
используя базовые комбинаторы `slider` и `selector`, а также интерфейс аппликативного функтора.

